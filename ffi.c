/*
   +----------------------------------------------------------------------+
   | PHP Version 7                                                        |
   +----------------------------------------------------------------------+
   | Copyright (c) 1997-2018 The PHP Group                                |
   +----------------------------------------------------------------------+
   | This source file is subject to version 3.01 of the PHP license,      |
   | that is bundled with this package in the file LICENSE, and is        |
   | available through the world-wide-web at the following url:           |
   | http://www.php.net/license/3_01.txt                                  |
   | If you did not receive a copy of the PHP license and are unable to   |
   | obtain it through the world-wide-web, please send a note to          |
   | license@php.net so we can mail you a copy immediately.               |
   +----------------------------------------------------------------------+
   | Author: Dmitry Stogov <dmitry@zend.com>                              |
   +----------------------------------------------------------------------+
*/

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "php.h"
#include "php_ffi.h"
#include "ext/standard/info.h"
#include "php_scandir.h"
#include "zend_exceptions.h"
#include "zend_interfaces.h"
#include "zend_closures.h"

#include <ffi.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

ZEND_DECLARE_MODULE_GLOBALS(ffi)

typedef enum _zend_ffi_tag_kind {
	ZEND_FFI_TAG_ENUM,
	ZEND_FFI_TAG_STRUCT,
	ZEND_FFI_TAG_UNION
} zend_ffi_tag_kind;

static const char *zend_ffi_tag_kind_name[3] = {"enum", "struct", "union"};


typedef struct _zend_ffi_tag {
	zend_ffi_tag_kind      kind;
	zend_ffi_type         *type;
} zend_ffi_tag;

typedef enum _zend_ffi_type_kind {
	ZEND_FFI_TYPE_VOID,
	ZEND_FFI_TYPE_FLOAT,
	ZEND_FFI_TYPE_DOUBLE,
#ifdef HAVE_LONG_DOUBLE
	ZEND_FFI_TYPE_LONGDOUBLE,
#endif
	ZEND_FFI_TYPE_UINT8,
	ZEND_FFI_TYPE_SINT8,
	ZEND_FFI_TYPE_UINT16,
	ZEND_FFI_TYPE_SINT16,
	ZEND_FFI_TYPE_UINT32,
	ZEND_FFI_TYPE_SINT32,
	ZEND_FFI_TYPE_UINT64,
	ZEND_FFI_TYPE_SINT64,
	ZEND_FFI_TYPE_ENUM,
	ZEND_FFI_TYPE_BOOL,
	ZEND_FFI_TYPE_CHAR,
	ZEND_FFI_TYPE_POINTER,
	ZEND_FFI_TYPE_FUNC,
	ZEND_FFI_TYPE_ARRAY,
	ZEND_FFI_TYPE_STRUCT,
} zend_ffi_type_kind;

typedef enum _zend_ffi_flags {
	ZEND_FFI_FLAG_OWNED      = (1 << 0),
	ZEND_FFI_FLAG_PERSISTENT = (1 << 1),
	ZEND_FFI_FLAG_CONST      = (1 << 2),
} zend_ffi_flags;

struct _zend_ffi_type {
	zend_ffi_type_kind     kind;
	size_t                 size;
	uint32_t               align;
	uint32_t               attr;
	union {
		struct {
			zend_ffi_type_kind kind;
		} enumeration;
		struct {
			zend_ffi_type *type;
			zend_long      length;
		} array;
		struct {
			zend_ffi_type *type;
		} pointer;
		struct {
			HashTable      fields;
		} record;
		struct {
			zend_ffi_type *ret_type;
			HashTable     *args;
			ffi_abi        abi;
		} func;
	};
};

typedef struct _zend_ffi_field {
	size_t                 offset;
	zend_bool              is_const;
	zend_bool              is_nested; /* part of nested anonymous struct */
	uint8_t                first_bit;
	uint8_t                bits;
	zend_ffi_type         *type;
} zend_ffi_field;

typedef enum _zend_ffi_symbol_kind {
	ZEND_FFI_SYM_TYPE,
	ZEND_FFI_SYM_CONST,
	ZEND_FFI_SYM_VAR,
	ZEND_FFI_SYM_FUNC
} zend_ffi_symbol_kind;

typedef struct _zend_ffi_symbol {
	zend_ffi_symbol_kind   kind;
	zend_bool              is_const;
	zend_ffi_type         *type;
	union {
		void *addr;
		int64_t value;
	};
} zend_ffi_symbol;

typedef struct _zend_ffi_scope {
	HashTable             *symbols;
	HashTable             *tags;
} zend_ffi_scope;

typedef struct _zend_ffi {
	zend_object            std;
	DL_HANDLE              lib;
	HashTable             *symbols;
	HashTable             *tags;
	zend_bool              persistent;
} zend_ffi;

#define ZEND_FFI_TYPE_OWNED        (1<<0)

#define ZEND_FFI_TYPE(t) \
	((zend_ffi_type*)(((uintptr_t)(t)) & ~ZEND_FFI_TYPE_OWNED))

#define ZEND_FFI_TYPE_IS_OWNED(t) \
	(((uintptr_t)(t)) & ZEND_FFI_TYPE_OWNED)

#define ZEND_FFI_TYPE_MAKE_OWNED(t) \
	((zend_ffi_type*)(((uintptr_t)(t)) | ZEND_FFI_TYPE_OWNED))

typedef struct _zend_ffi_cdata {
	zend_object            std;
	zend_ffi_type         *type;
	void                  *ptr;
	void                  *ptr_holder;
	zend_ffi_flags         flags;
} zend_ffi_cdata;

typedef struct _zend_ffi_ctype {
	zend_object            std;
	zend_ffi_type         *type;
} zend_ffi_ctype;

static zend_class_entry *zend_ffi_exception_ce;
static zend_class_entry *zend_ffi_parser_exception_ce;
static zend_class_entry *zend_ffi_ce;
static zend_class_entry *zend_ffi_cdata_ce;
static zend_class_entry *zend_ffi_ctype_ce;

static zend_object_handlers zend_ffi_handlers;
static zend_object_handlers zend_ffi_cdata_handlers;
static zend_object_handlers zend_ffi_cdata_value_handlers;
static zend_object_handlers zend_ffi_ctype_handlers;

static zend_internal_function zend_ffi_new_fn;
static zend_internal_function zend_ffi_cast_fn;
static zend_internal_function zend_ffi_type_fn;

/* forward declarations */
static void zend_ffi_type_dtor(zend_ffi_type *type);
static void zend_ffi_finalize_type(zend_ffi_dcl *dcl);
static int zend_ffi_zval_to_cdata(void *ptr, zend_ffi_type *type, zval *value);
static char *zend_ffi_parse_directives(const char *filename, char *code_pos, char **scope_name, char **lib, zend_bool preload);
static ZEND_FUNCTION(ffi_trampoline);

static zend_object *zend_ffi_cdata_new(zend_class_entry *class_type) /* {{{ */
{
	zend_ffi_cdata *cdata;

	cdata = emalloc(sizeof(zend_ffi_cdata));
	memset(cdata, 0, sizeof(zend_ffi_cdata));

	zend_object_std_init(&cdata->std, class_type);
	cdata->std.handlers = &zend_ffi_cdata_handlers;

	cdata->type = NULL;
	cdata->ptr = NULL;
	cdata->flags = 0;

	return &cdata->std;
}
/* }}} */

static int zend_ffi_is_compatible_type(zend_ffi_type *dst_type, zend_ffi_type *src_type) /* {{{ */
{
	while (1) {
		if (dst_type == src_type) {
			return 1;
		} else if (dst_type->kind == src_type->kind) {
			if (dst_type->kind < ZEND_FFI_TYPE_POINTER) {
				return 1;
			} else if (dst_type->kind == ZEND_FFI_TYPE_POINTER) {
				dst_type = ZEND_FFI_TYPE(dst_type->pointer.type);
				src_type = ZEND_FFI_TYPE(src_type->pointer.type);
				if (dst_type->kind == ZEND_FFI_TYPE_VOID ||
				    src_type->kind == ZEND_FFI_TYPE_VOID) {
				    return 1;
				}
			} else if (dst_type->kind == ZEND_FFI_TYPE_ARRAY &&
			           (dst_type->array.length == src_type->array.length ||
			            dst_type->array.length == 0)) {
				dst_type = ZEND_FFI_TYPE(dst_type->array.type);
				src_type = ZEND_FFI_TYPE(src_type->array.type);
			} else {
				break;
			}
		} else if (dst_type->kind == ZEND_FFI_TYPE_POINTER &&
		           src_type->kind == ZEND_FFI_TYPE_ARRAY) {
			dst_type = ZEND_FFI_TYPE(dst_type->pointer.type);
			src_type = ZEND_FFI_TYPE(src_type->array.type);
			if (dst_type->kind == ZEND_FFI_TYPE_VOID) {
			    return 1;
			}
		} else {
			break;
		}
	}
	return 0;
}
/* }}} */

static ffi_type *zend_ffi_make_fake_struct_type(zend_ffi_type *type) /* {{{ */
{
	ffi_type *t = emalloc(sizeof(ffi_type) + sizeof(ffi_type*) * (zend_hash_num_elements(&type->record.fields) + 1));
	int i;
	zend_ffi_field *field;

	t->size = type->size;
	t->alignment = type->align;
	t->type = FFI_TYPE_STRUCT;
	t->elements = (ffi_type**)(t + 1);
	i = 0;
	ZEND_HASH_FOREACH_PTR(&type->record.fields, field) {
		switch (ZEND_FFI_TYPE(field->type)->kind) {
			case ZEND_FFI_TYPE_FLOAT:
				t->elements[i] = &ffi_type_float;
				break;
			case ZEND_FFI_TYPE_DOUBLE:
				t->elements[i] = &ffi_type_double;
				break;
#ifndef PHP_WIN32
			case ZEND_FFI_TYPE_LONGDOUBLE:
				t->elements[i] = &ffi_type_longdouble;
				break;
#endif
			case ZEND_FFI_TYPE_SINT8:
			case ZEND_FFI_TYPE_UINT8:
			case ZEND_FFI_TYPE_BOOL:
			case ZEND_FFI_TYPE_CHAR:
				t->elements[i] = &ffi_type_uint8;
				break;
			case ZEND_FFI_TYPE_SINT16:
			case ZEND_FFI_TYPE_UINT16:
				t->elements[i] = &ffi_type_uint16;
				break;
			case ZEND_FFI_TYPE_SINT32:
			case ZEND_FFI_TYPE_UINT32:
				t->elements[i] = &ffi_type_uint32;
				break;
			case ZEND_FFI_TYPE_SINT64:
			case ZEND_FFI_TYPE_UINT64:
				t->elements[i] = &ffi_type_uint64;
				break;
			case ZEND_FFI_TYPE_POINTER:
				t->elements[i] = &ffi_type_pointer;
				break;
			default:
				efree(t);
				zend_throw_error(zend_ffi_exception_ce, "Passing incompatible struct/union");
				return NULL;
			}
		i++;
	} ZEND_HASH_FOREACH_END();
	t->elements[i] = NULL;
	return t;
}
/* }}} */

static ffi_type *zend_ffi_get_type(zend_ffi_type *type) /* {{{ */
{
	zend_ffi_type_kind kind = type->kind;

again:
    switch (kind) {
		case ZEND_FFI_TYPE_FLOAT:
			return &ffi_type_float;
		case ZEND_FFI_TYPE_DOUBLE:
			return &ffi_type_double;
#ifndef PHP_WIN32
		case ZEND_FFI_TYPE_LONGDOUBLE:
			return &ffi_type_longdouble;
#endif
		case ZEND_FFI_TYPE_UINT8:
			return &ffi_type_uint8;
		case ZEND_FFI_TYPE_SINT8:
			return &ffi_type_sint8;
		case ZEND_FFI_TYPE_UINT16:
			return &ffi_type_uint16;
		case ZEND_FFI_TYPE_SINT16:
			return &ffi_type_sint16;
		case ZEND_FFI_TYPE_UINT32:
			return &ffi_type_uint32;
		case ZEND_FFI_TYPE_SINT32:
			return &ffi_type_sint32;
		case ZEND_FFI_TYPE_UINT64:
			return &ffi_type_uint64;
		case ZEND_FFI_TYPE_SINT64:
			return &ffi_type_sint64;
		case ZEND_FFI_TYPE_POINTER:
			return &ffi_type_pointer;
		case ZEND_FFI_TYPE_VOID:
			return &ffi_type_void;
		case ZEND_FFI_TYPE_BOOL:
			return &ffi_type_uint8;
		case ZEND_FFI_TYPE_CHAR:
			return &ffi_type_sint8;
		case ZEND_FFI_TYPE_ENUM:
			kind = type->enumeration.kind;
			goto again;
		case ZEND_FFI_TYPE_STRUCT:
			if (!(type->attr & ZEND_FFI_ATTR_UNION)) {
				ffi_type *t = zend_ffi_make_fake_struct_type(type);
				if (t) {
					return t;
				}
			}
			zend_throw_error(zend_ffi_exception_ce, "FFI return struct/union is not implemented");
			break;
		case ZEND_FFI_TYPE_ARRAY:
			zend_throw_error(zend_ffi_exception_ce, "FFI return array is not implemented");
			break;
		default:
			zend_throw_error(zend_ffi_exception_ce, "FFI internal error");
			break;
	}
	return NULL;
}
/* }}} */

static int zend_ffi_cdata_to_zval(zend_ffi_cdata *cdata, void *ptr, zend_ffi_type *type, int read_type, zval *rv, zend_bool is_const, zend_bool is_ret) /* {{{ */
{
	if (read_type == BP_VAR_R && type->kind < ZEND_FFI_TYPE_ARRAY) {
		zend_ffi_type_kind kind = type->kind;

again:
	    switch (kind) {
			case ZEND_FFI_TYPE_FLOAT:
				ZVAL_DOUBLE(rv, *(float*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_DOUBLE:
				ZVAL_DOUBLE(rv, *(double*)ptr);
				return SUCCESS;
#ifdef HAVE_LONG_DOUBLE
			case ZEND_FFI_TYPE_LONGDOUBLE:
				ZVAL_DOUBLE(rv, *(long double*)ptr);
				return SUCCESS;
#endif
			case ZEND_FFI_TYPE_UINT8:
				ZVAL_LONG(rv, *(uint8_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_SINT8:
				ZVAL_LONG(rv, *(int8_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_UINT16:
				ZVAL_LONG(rv, *(uint16_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_SINT16:
				ZVAL_LONG(rv, *(int16_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_UINT32:
				ZVAL_LONG(rv, *(uint32_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_SINT32:
				ZVAL_LONG(rv, *(int32_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_UINT64:
				ZVAL_LONG(rv, *(uint64_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_SINT64:
				ZVAL_LONG(rv, *(int64_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_BOOL:
				ZVAL_BOOL(rv, *(uint8_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_CHAR:
				ZVAL_INTERNED_STR(rv, ZSTR_CHAR(*(unsigned char*)ptr));
				return SUCCESS;
			case ZEND_FFI_TYPE_ENUM:
				kind = type->enumeration.kind;
				goto again;
			case ZEND_FFI_TYPE_POINTER:
				if (*(void**)ptr == NULL) {
					ZVAL_NULL(rv);
					return SUCCESS;
				} else if ((type->attr & ZEND_FFI_ATTR_CONST) && ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_CHAR) {
					ZVAL_STRING(rv, *(char**)ptr);
					return SUCCESS;
				}
				break;
			default:
				break;
		}
	}

	if (!cdata) {
		cdata = (zend_ffi_cdata*)zend_ffi_cdata_new(zend_ffi_cdata_ce);
		if (type->kind < ZEND_FFI_TYPE_POINTER) {
			cdata->std.handlers = &zend_ffi_cdata_value_handlers;
		}
		cdata->type = type;
		if ((is_ret || read_type == BP_VAR_R) && type->kind == ZEND_FFI_TYPE_POINTER) {
			cdata->ptr = (void*)&cdata->ptr_holder;
			*(void**)cdata->ptr = *(void**)ptr;
		} else if (is_ret && type->kind == ZEND_FFI_TYPE_STRUCT) {
			cdata->ptr = emalloc(type->size);
			cdata->flags |= ZEND_FFI_FLAG_OWNED;
			memcpy(cdata->ptr, ptr, type->size);
		} else {
			cdata->ptr = ptr;
		}
		if (is_const) {
			cdata->flags |= ZEND_FFI_FLAG_CONST;
		}
	} else {
		GC_ADDREF(&cdata->std);
	}

	ZVAL_OBJ(rv, &cdata->std);
	return SUCCESS;
}
/* }}} */

static void zend_ffi_bit_field_to_zval(void *ptr, zend_ffi_field *field, zval *rv) /* {{{ */
{
	uint64_t *p1 = (uint64_t *)((char*)ptr + (field->first_bit / 64) * 8);
	uint64_t *p2 = (uint64_t *)((char*)ptr + ((field->first_bit + field->bits - 1) / 64) * 8);
	uint64_t pos = field->first_bit % 64;
	uint64_t shift = 64 - (field->bits % 64);
	uint64_t val;

	if (p1 == p2) {
		if (field->bits == 64) {
			val = *p1;
			shift = 0;
		} else {
			val = *p1 << (shift - pos);
		}
	} else {
		val = (*p1 >> pos) | (*p2 << (64 - pos));
	}
	if (ZEND_FFI_TYPE(field->type)->kind == ZEND_FFI_TYPE_CHAR
	 || ZEND_FFI_TYPE(field->type)->kind == ZEND_FFI_TYPE_SINT8
	 || ZEND_FFI_TYPE(field->type)->kind == ZEND_FFI_TYPE_SINT16
	 || ZEND_FFI_TYPE(field->type)->kind == ZEND_FFI_TYPE_SINT32
	 || ZEND_FFI_TYPE(field->type)->kind == ZEND_FFI_TYPE_SINT64) {
		val = (int64_t)val >> shift;
	} else {
		val = val >> shift;
	}
	ZVAL_LONG(rv, val);
}
/* }}} */

static int zend_ffi_zval_to_bit_field(void *ptr, zend_ffi_field *field, zval *value) /* {{{ */
{
	uint64_t *p1 = (uint64_t *)((char*)ptr + (field->first_bit / 64) * 8);
	uint64_t *p2 = (uint64_t *)((char*)ptr + ((field->first_bit + field->bits - 1) / (8 * 8)) * 8);
	uint64_t pos = field->first_bit % 64;
	uint64_t mask;
	uint64_t val = zval_get_long(value);

	if (p1 == p2) {
		if (field->bits == 64) {
			*p1 = val;
		} else {
			mask = ((1ULL << field->bits) - 1ULL) << pos;
			*p1 = (*p1 & ~mask) | (val << pos) & mask;
		}
	} else {
		mask = ((1ULL << (64 - pos)) - 1ULL) << pos;
		*p1 = (*p1 & ~mask) | (val << pos) & mask;
		mask = (1ULL << pos) - 1ULL;
		*p2 = (*p2 & ~mask) | (val >> (64 - pos)) & mask;
	}
	return SUCCESS;
}
/* }}} */

#if FFI_CLOSURES
typedef struct _zend_ffi_callback_data {
	zend_fcall_info_cache  fcc;
	zend_ffi_type         *type;
	void                  *code;
	void                  *callback;
	ffi_cif                cif;
	uint32_t               arg_count;
	ffi_type              *ret_type;
	ffi_type              *arg_types[0];
} zend_ffi_callback_data;

static void zend_ffi_callback_hash_dtor(zval *zv) /* {{{ */
{
	zend_ffi_callback_data *callback_data = Z_PTR_P(zv);

	ffi_closure_free(callback_data->callback);
	if (callback_data->fcc.function_handler->common.fn_flags & ZEND_ACC_CLOSURE) {
		OBJ_RELEASE(ZEND_CLOSURE_OBJECT(callback_data->fcc.function_handler));
	}
	efree(callback_data);
}
/* }}} */

static void zend_ffi_callback_trampoline(ffi_cif* cif, void* ret, void** args, void* data) /* {{{ */
{
	zend_ffi_callback_data *callback_data = (zend_ffi_callback_data*)data;
	zend_fcall_info fci;
	zval retval;
	ALLOCA_FLAG(use_heap)

	fci.size = sizeof(zend_fcall_info);
	ZVAL_UNDEF(&fci.function_name);
	fci.retval = &retval;
	fci.params = do_alloca(sizeof(zval) *callback_data->arg_count, use_heap);
	fci.object = NULL;
	fci.no_separation = 1;
	fci.param_count = callback_data->arg_count;

	if (callback_data->type->func.args) {
		int n = 0;
		zend_ffi_type *arg_type;

		ZEND_HASH_FOREACH_PTR(callback_data->type->func.args, arg_type) {
			arg_type = ZEND_FFI_TYPE(arg_type);
			zend_ffi_cdata_to_zval(NULL, args[n], arg_type, BP_VAR_R, &fci.params[n], (arg_type->attr & ZEND_FFI_ATTR_CONST), 0);
			n++;
		} ZEND_HASH_FOREACH_END();
	}

	ZVAL_UNDEF(&retval);
	if (zend_call_function(&fci, &callback_data->fcc) != SUCCESS) {
		zend_throw_error(zend_ffi_exception_ce, "Cannot call callback");
	}

	if (callback_data->arg_count) {
		int n = 0;

		for (n = 0; n < callback_data->arg_count; n++) {
			zval_ptr_dtor(&fci.params[n]);
			n++;
		}
	}
	free_alloca(fci.params, use_heap);

	zend_ffi_zval_to_cdata(ret, ZEND_FFI_TYPE(callback_data->type->func.ret_type), &retval);
}
/* }}} */

static void *zend_ffi_create_callback(zend_ffi_type *type, zval *value) /* {{{ */
{
	zend_fcall_info_cache fcc;
	char *error = NULL;
	uint32_t arg_count;
	void *code;
	void *callback;
	zend_ffi_callback_data *callback_data;

	if (type->attr & ZEND_FFI_ATTR_VARIADIC) {
		zend_throw_error(zend_ffi_exception_ce, "Variadic function closures are not supported");
		return NULL;
	}

	if (!zend_is_callable_ex(value, NULL, 0, NULL, &fcc, &error)) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to assing an invalid callback, %s", error);
		return NULL;
	}

	arg_count = type->func.args ? zend_hash_num_elements(type->func.args) : 0;
	if (arg_count < fcc.function_handler->common.required_num_args) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to assing an invalid callback, insufficient number of arguments");
		return NULL;
	}

	callback = ffi_closure_alloc(sizeof(ffi_closure), &code);
	if (!callback) {
		zend_throw_error(zend_ffi_exception_ce, "Cannot allocate callback");
		return NULL;
	}

	callback_data = emalloc(sizeof(zend_ffi_callback_data) + sizeof(ffi_type*) * arg_count);
	memcpy(&callback_data->fcc, &fcc, sizeof(zend_fcall_info_cache));
	callback_data->type = type;
	callback_data->callback = callback;
	callback_data->code = code;
	callback_data->arg_count = arg_count;

	if (type->func.args) {
		int n = 0;
		zend_ffi_type *arg_type;

		ZEND_HASH_FOREACH_PTR(type->func.args, arg_type) {
			arg_type = ZEND_FFI_TYPE(arg_type);
			callback_data->arg_types[n] = zend_ffi_get_type(arg_type);
			if (!callback_data->arg_types[n]) {
				efree(callback_data);
				ffi_closure_free(callback);
				return NULL;
			}
			n++;
		} ZEND_HASH_FOREACH_END();
	}
	callback_data->ret_type = zend_ffi_get_type(type->func.ret_type);
	if (!callback_data->ret_type) {
		efree(callback_data);
		ffi_closure_free(callback);
		return NULL;
	}

	if (ffi_prep_cif(&callback_data->cif, type->func.abi, callback_data->arg_count, callback_data->ret_type, callback_data->arg_types) != FFI_OK) {
		zend_throw_error(zend_ffi_exception_ce, "Cannot prepare callback CIF");
		efree(callback_data);
		ffi_closure_free(callback);
		return NULL;
	}

	if (ffi_prep_closure_loc(callback, &callback_data->cif, zend_ffi_callback_trampoline, callback_data, code) != FFI_OK) {
		zend_throw_error(zend_ffi_exception_ce, "Cannot prepare callback");
		efree(callback_data);
		ffi_closure_free(callback);
		return NULL;
	}

	if (!FFI_G(callbacks)) {
		FFI_G(callbacks) = emalloc(sizeof(HashTable));
		zend_hash_init(FFI_G(callbacks), 0, NULL, zend_ffi_callback_hash_dtor, 0);
	}
	zend_hash_next_index_insert_ptr(FFI_G(callbacks), callback_data);

	if (fcc.function_handler->common.fn_flags & ZEND_ACC_CLOSURE) {
		GC_ADDREF(ZEND_CLOSURE_OBJECT(fcc.function_handler));
	}

	return code;
}
/* }}} */
#endif

static int zend_ffi_zval_to_cdata(void *ptr, zend_ffi_type *type, zval *value) /* {{{ */
{
	zend_long lval;
	double dval;
	zend_string *tmp_str;
	zend_string *str;
	zend_ffi_type_kind kind = type->kind;

again:
    switch (kind) {
		case ZEND_FFI_TYPE_FLOAT:
			dval = zval_get_double(value);
			*(float*)ptr = dval;
			break;
		case ZEND_FFI_TYPE_DOUBLE:
			dval = zval_get_double(value);
			*(double*)ptr = dval;
			break;
#ifdef HAVE_LONG_DOUBLE
		case ZEND_FFI_TYPE_LONGDOUBLE:
			dval = zval_get_double(value);
			*(long double*)ptr = dval;
			break;
#endif
		case ZEND_FFI_TYPE_UINT8:
			lval = zval_get_long(value);
			*(uint8_t*)ptr = lval;
			break;
		case ZEND_FFI_TYPE_SINT8:
			lval = zval_get_long(value);
			*(int8_t*)ptr = lval;
			break;
		case ZEND_FFI_TYPE_UINT16:
			lval = zval_get_long(value);
			*(uint16_t*)ptr = lval;
			break;
		case ZEND_FFI_TYPE_SINT16:
			lval = zval_get_long(value);
			*(int16_t*)ptr = lval;
			break;
		case ZEND_FFI_TYPE_UINT32:
			lval = zval_get_long(value);
			*(uint32_t*)ptr = lval;
			break;
		case ZEND_FFI_TYPE_SINT32:
			lval = zval_get_long(value);
			*(int32_t*)ptr = lval;
			break;
		case ZEND_FFI_TYPE_UINT64:
			lval = zval_get_long(value);
			*(uint64_t*)ptr = lval;
			break;
		case ZEND_FFI_TYPE_SINT64:
			lval = zval_get_long(value);
			*(int64_t*)ptr = lval;
			break;
		case ZEND_FFI_TYPE_BOOL:
			*(uint8_t*)ptr = zend_is_true(value);
			break;
		case ZEND_FFI_TYPE_CHAR:
			str = zval_get_tmp_string(value, &tmp_str);
			if (ZSTR_LEN(str) == 1) {
				*(char*)ptr = ZSTR_VAL(str)[0];
			} else {
				zend_throw_error(zend_ffi_exception_ce, "Attempt to perform assign of incompatible C type");
				return FAILURE;
			}
			zend_tmp_string_release(tmp_str);
			break;
		case ZEND_FFI_TYPE_ENUM:
			kind = type->enumeration.kind;
			goto again;
		case ZEND_FFI_TYPE_POINTER:
			if (Z_TYPE_P(value) == IS_NULL) {
				*(void**)ptr = NULL;
				break;
			} else if (Z_TYPE_P(value) == IS_OBJECT && Z_OBJCE_P(value) == zend_ffi_cdata_ce) {
				zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(value);

				if (zend_ffi_is_compatible_type(type, ZEND_FFI_TYPE(cdata->type))) {
					if (ZEND_FFI_TYPE(cdata->type)->kind == ZEND_FFI_TYPE_POINTER) {
						*(void**)ptr = *(void**)cdata->ptr;
					} else {
						if (cdata->flags & ZEND_FFI_FLAG_OWNED) {
							zend_throw_error(zend_ffi_exception_ce, "Attempt to perform assign of owned C pointer");
							return FAILURE;
						}
						*(void**)ptr = cdata->ptr;
					}
					return SUCCESS;
				}
#if FFI_CLOSURES
			} else if (ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_FUNC) {
				void *callback = zend_ffi_create_callback(ZEND_FFI_TYPE(type->pointer.type), value);

				if (callback) {
					*(void**)ptr = callback;
					break;
				} else {
					return FAILURE;
				}
#endif
			}
			zend_throw_error(zend_ffi_exception_ce, "Attempt to perform assign of incompatible C type");
			return FAILURE;
		case ZEND_FFI_TYPE_STRUCT:
		case ZEND_FFI_TYPE_ARRAY:
		default:
			if (Z_TYPE_P(value) == IS_OBJECT && Z_OBJCE_P(value) == zend_ffi_cdata_ce) {
				zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(value);
				if (zend_ffi_is_compatible_type(type, ZEND_FFI_TYPE(cdata->type)) &&
				    type->size == ZEND_FFI_TYPE(cdata->type)->size) {
					memcpy(ptr, cdata->ptr, type->size);
					return SUCCESS;
				}
			}
			zend_throw_error(zend_ffi_exception_ce, "Attempt to perform assign of incompatible C type");
			return FAILURE;
	}
	return SUCCESS;
}
/* }}} */

static zval* zend_ffi_cdata_get(zval *object, zval *rv) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);

	if (!cdata->ptr) {
		zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
		return &EG(uninitialized_zval);
	}

	if (zend_ffi_cdata_to_zval(cdata, cdata->ptr, type, BP_VAR_R, rv, (cdata->flags & ZEND_FFI_FLAG_CONST) != 0, 0) != SUCCESS) {
		return &EG(uninitialized_zval);
	}

	return rv;
}
/* }}} */

static void zend_ffi_cdata_set(zval *object, zval *value) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);

	if (!cdata->ptr) {
		zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
		return;
	}

	zend_ffi_zval_to_cdata(cdata->ptr, type, value);
}
/* }}} */

static int zend_ffi_cdata_cast_object(zval *readobj, zval *writeobj, int type) /* {{{ */
{
	return FAILURE;
}
/* }}} */

static zval *zend_ffi_cdata_read_field(zval *object, zval *member, int read_type, void **cache_slot, zval *rv) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	void           *ptr;
	zend_ffi_field *field;

	if (cache_slot && *cache_slot == type) {
		field = *(cache_slot + 1);
	} else {
		zend_string *tmp_field_name;
		zend_string *field_name = zval_get_tmp_string(member, &tmp_field_name);

		if (type->kind != ZEND_FFI_TYPE_STRUCT) {
			zend_throw_error(zend_ffi_exception_ce, "Attempt to read field '%s' of non C struct/union", ZSTR_VAL(field_name));
			zend_tmp_string_release(tmp_field_name);
			return &EG(uninitialized_zval);
		}

		field = zend_hash_find_ptr(&type->record.fields, field_name);
		if (!field) {
			zend_throw_error(zend_ffi_exception_ce, "Attempt to read undefined field '%s' of C struct/union", ZSTR_VAL(field_name));
			zend_tmp_string_release(tmp_field_name);
			return &EG(uninitialized_zval);
		}

		zend_tmp_string_release(tmp_field_name);

		if (cache_slot) {
			*cache_slot = type;
			*(cache_slot + 1) = field;
		}
	}

	if (!cdata->ptr) {
		zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
		return &EG(uninitialized_zval);
	}

	if (!field->bits) {
		ptr = (void*)(((char*)cdata->ptr) + field->offset);
		if (zend_ffi_cdata_to_zval(NULL, ptr, ZEND_FFI_TYPE(field->type), read_type, rv, (cdata->flags & ZEND_FFI_FLAG_CONST) || field->is_const, 0) != SUCCESS) {
			return &EG(uninitialized_zval);
		}
	} else {
		zend_ffi_bit_field_to_zval(cdata->ptr, field, rv);
	}

	return rv;
}
/* }}} */

static void zend_ffi_cdata_write_field(zval *object, zval *member, zval *value, void **cache_slot) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	void           *ptr;
	zend_ffi_field *field;

	if (cache_slot && *cache_slot == type) {
		field = *(cache_slot + 1);
	} else {
		zend_string *tmp_field_name;
		zend_string *field_name = zval_get_tmp_string(member, &tmp_field_name);

		if (type->kind != ZEND_FFI_TYPE_STRUCT) {
			zend_throw_error(zend_ffi_exception_ce, "Attempt to assign field '%s' of non C struct/union", ZSTR_VAL(field_name));
			zend_tmp_string_release(tmp_field_name);
			return;
		}

		field = zend_hash_find_ptr(&type->record.fields, field_name);
		if (!field) {
			zend_throw_error(zend_ffi_exception_ce, "Attempt to assign undefined field '%s' of C struct/union", ZSTR_VAL(field_name));
			zend_tmp_string_release(tmp_field_name);
			return;
		}

		zend_tmp_string_release(tmp_field_name);

		if (cache_slot) {
			*cache_slot = type;
			*(cache_slot + 1) = field;
		}
	}

	if (!cdata->ptr) {
		zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
		return;
	}

	if (cdata->flags & ZEND_FFI_FLAG_CONST) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to assign read-only location");
		return;
	} else if (field->is_const) {
		zend_string *tmp_field_name;
		zend_string *field_name = zval_get_tmp_string(member, &tmp_field_name);
		zend_throw_error(zend_ffi_exception_ce, "Attempt to assign read-only field '%s'", ZSTR_VAL(field_name));
		zend_tmp_string_release(tmp_field_name);
		return;
	}

	if (!field->bits) {
		ptr = (void*)(((char*)cdata->ptr) + field->offset);
		zend_ffi_zval_to_cdata(ptr, ZEND_FFI_TYPE(field->type), value);
	} else {
		zend_ffi_zval_to_bit_field(cdata->ptr, field, value);
	}
}
/* }}} */

static zval *zend_ffi_cdata_read_dim(zval *object, zval *offset, int read_type, zval *rv) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	zend_long       dim = zval_get_long(offset);
	void           *ptr;
	zend_bool       is_const;

	if (type->kind == ZEND_FFI_TYPE_ARRAY) {
		if (dim < 0 || (type->array.length && dim >= type->array.length)) {
			zend_throw_error(zend_ffi_exception_ce, "C array index out of bounds");
			return &EG(uninitialized_zval);
		}

		is_const = (cdata->flags & ZEND_FFI_FLAG_CONST) || (type->attr & ZEND_FFI_ATTR_CONST);
		type = ZEND_FFI_TYPE(type->array.type);
		if (!cdata->ptr) {
			zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
			return &EG(uninitialized_zval);
		}
		ptr = (void*)(((char*)cdata->ptr) + type->size * dim);
	} else if (type->kind == ZEND_FFI_TYPE_POINTER) {
		is_const = (cdata->flags & ZEND_FFI_FLAG_CONST) || (type->attr & ZEND_FFI_ATTR_CONST);
		type = ZEND_FFI_TYPE(type->pointer.type);
		if (!cdata->ptr) {
			zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
			return &EG(uninitialized_zval);
		}
		ptr = (void*)((*(char**)cdata->ptr) + type->size * dim);
	} else {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to read element of non C array");
		return &EG(uninitialized_zval);
	}

	if (zend_ffi_cdata_to_zval(NULL, ptr, type, read_type, rv, is_const, 0) != SUCCESS) {
		return &EG(uninitialized_zval);
	}

	return rv;
}
/* }}} */

static void zend_ffi_cdata_write_dim(zval *object, zval *offset, zval *value) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	zend_long       dim = zval_get_long(offset);
	void           *ptr;
	zend_bool       is_const;

	if (type->kind == ZEND_FFI_TYPE_ARRAY) {
		if (dim < 0 || (type->array.length && dim >= type->array.length)) {
			zend_throw_error(zend_ffi_exception_ce, "C array index out of bounds");
			return;
		}

		is_const = (cdata->flags & ZEND_FFI_FLAG_CONST) || (type->attr & ZEND_FFI_ATTR_CONST);
		type = ZEND_FFI_TYPE(type->array.type);
		if (!cdata->ptr) {
			zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
			return;
		}
		ptr = (void*)(((char*)cdata->ptr) + type->size * dim);
	} else if (type->kind == ZEND_FFI_TYPE_POINTER) {
		is_const = (cdata->flags & ZEND_FFI_FLAG_CONST) || (type->attr & ZEND_FFI_ATTR_CONST);
		type = ZEND_FFI_TYPE(type->pointer.type);
		if (!cdata->ptr) {
			zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
			return;
		}
		ptr = (void*)((*(char**)cdata->ptr) + type->size * dim);
	} else {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to assign element of non C array");
		return;
	}

	if (is_const) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to assign read-only location");
		return;
	}

	zend_ffi_zval_to_cdata(ptr, type, value);
}
/* }}} */

#define MAX_TYPE_NAME_LEN 256

typedef struct _zend_ffi_ctype_name_buf {
	char *start;
	char *end;
	char buf[MAX_TYPE_NAME_LEN];
} zend_ffi_ctype_name_buf;

static int zend_ffi_ctype_name_prepend(zend_ffi_ctype_name_buf *buf, const char *str, size_t len) /* {{{ */
{
	buf->start -= len;
	if (buf->start < buf->buf) {
		return 0;
	}
	memcpy(buf->start, str, len);
	return 1;
}
/* }}} */

static int zend_ffi_ctype_name_append(zend_ffi_ctype_name_buf *buf, const char *str, size_t len) /* {{{ */
{
	if (buf->end + len > buf->buf + MAX_TYPE_NAME_LEN) {
		return 0;
	}
	memcpy(buf->end, str, len);
	buf->end += len;
	return 1;
}
/* }}} */

static int zend_ffi_ctype_name(zend_ffi_ctype_name_buf *buf, const zend_ffi_type *type) /* {{{ */
{
	const char *name = NULL;
	int is_ptr = 0;

	while (1) {
		switch (type->kind) {
			case ZEND_FFI_TYPE_VOID:
				name = "void";
				break;
			case ZEND_FFI_TYPE_FLOAT:
				name = "float";
				break;
			case ZEND_FFI_TYPE_DOUBLE:
				name = "double";
				break;
#ifdef HAVE_LONG_DOUBLE
			case ZEND_FFI_TYPE_LONGDOUBLE:
				name = "long double";
				break;
#endif
			case ZEND_FFI_TYPE_UINT8:
				name = "uint8_t";
				break;
			case ZEND_FFI_TYPE_SINT8:
				name = "int8_t";
				break;
			case ZEND_FFI_TYPE_UINT16:
				name = "uint16_t";
				break;
			case ZEND_FFI_TYPE_SINT16:
				name = "int16_t";
				break;
			case ZEND_FFI_TYPE_UINT32:
				name = "uint32_t";
				break;
			case ZEND_FFI_TYPE_SINT32:
				name = "int32_t";
				break;
			case ZEND_FFI_TYPE_UINT64:
				name = "uint64_t";
				break;
			case ZEND_FFI_TYPE_SINT64:
				name = "int64_t";
				break;
			case ZEND_FFI_TYPE_ENUM:
				name = "<enum>";
				break;
			case ZEND_FFI_TYPE_BOOL:
				name = "bool";
				break;
			case ZEND_FFI_TYPE_CHAR:
				name = "char";
				break;
			case ZEND_FFI_TYPE_POINTER:
				if (!zend_ffi_ctype_name_prepend(buf, "*", 1)) {
					return 0;
				}
				is_ptr = 1;
				type = ZEND_FFI_TYPE(type->pointer.type);
				break;
			case ZEND_FFI_TYPE_FUNC:
				if (is_ptr) {
					is_ptr = 0;
					if (!zend_ffi_ctype_name_prepend(buf, "(", 1)
					 || !zend_ffi_ctype_name_append(buf, ")", 1)) {
						return 0;
					}
				}
				if (!zend_ffi_ctype_name_append(buf, "(", 1)
				 || !zend_ffi_ctype_name_append(buf, ")", 1)) {
					return 0;
				}
				type = ZEND_FFI_TYPE(type->func.ret_type);
				break;
			case ZEND_FFI_TYPE_ARRAY:
				if (is_ptr) {
					is_ptr = 0;
					if (!zend_ffi_ctype_name_prepend(buf, "(", 1)
					 || !zend_ffi_ctype_name_append(buf, ")", 1)) {
						return 0;
					}
				}
				if (!zend_ffi_ctype_name_append(buf, "[", 1)) {
					return 0;
				}
				if (type->attr & ZEND_FFI_ATTR_VLA) {
					if (!zend_ffi_ctype_name_append(buf, "*", 1)) {
						return 0;
					}
				} else if (!(type->attr & ZEND_FFI_ATTR_INCOMPLETE_ARRAY)) {
					char str[MAX_LENGTH_OF_LONG + 1];
					char *s = zend_print_long_to_buf(str + sizeof(str) - 1, type->array.length);

					if (!zend_ffi_ctype_name_append(buf, s, strlen(s))) {
						return 0;
					}
				}
				if (!zend_ffi_ctype_name_append(buf, "]", 1)) {
					return 0;
				}
				type = ZEND_FFI_TYPE(type->array.type);
				break;
			case ZEND_FFI_TYPE_STRUCT:
				if (type->attr & ZEND_FFI_ATTR_UNION) {
					name = "<union>";
				} else {
					name = "<struct>";
				}
				break;
			default:
				ZEND_ASSERT(0);
		}
		if (name) {
			break;
		}
	}

//	if (buf->start != buf->end && *buf->start != '[') {
//		if (!zend_ffi_ctype_name_prepend(buf, " ", 1)) {
//			return 0;
//		}
//	}
	return zend_ffi_ctype_name_prepend(buf, name, strlen(name));
}
/* }}} */

static zend_string *zend_ffi_get_class_name(zend_string *prefix, const zend_ffi_type *type) /* {{{ */
{
	zend_ffi_ctype_name_buf buf;

	buf.start = buf.end = buf.buf + ((MAX_TYPE_NAME_LEN * 3) / 4);
	if (!zend_ffi_ctype_name(&buf, type)) {
		return zend_string_copy(prefix);
	} else {
		zend_string *name = zend_string_alloc(ZSTR_LEN(prefix) + 1 + buf.end - buf.start, 0);
		memcpy(ZSTR_VAL(name), ZSTR_VAL(prefix), ZSTR_LEN(prefix));
		ZSTR_VAL(name)[ZSTR_LEN(prefix)] = ':';
		memcpy(ZSTR_VAL(name) + ZSTR_LEN(prefix) + 1, buf.start, buf.end - buf.start);
		ZSTR_VAL(name)[ZSTR_LEN(name)] = 0;
		return name;
	}
}
/* }}} */

static zend_string *zend_ffi_cdata_get_class_name(const zend_object *zobj) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)zobj;

	return zend_ffi_get_class_name(zobj->ce->name, ZEND_FFI_TYPE(cdata->type));
}
/* }}} */

static int zend_ffi_cdata_compare_objects(zval *o1, zval *o2) /* {{{ */
{
	if (Z_TYPE_P(o1) == IS_OBJECT && Z_OBJCE_P(o1) == zend_ffi_cdata_ce &&
	    Z_TYPE_P(o2) == IS_OBJECT && Z_OBJCE_P(o2) == zend_ffi_cdata_ce) {
		zend_ffi_cdata *cdata1 = (zend_ffi_cdata*)Z_OBJ_P(o1);
		zend_ffi_cdata *cdata2 = (zend_ffi_cdata*)Z_OBJ_P(o2);
		zend_ffi_type *type1 = ZEND_FFI_TYPE(cdata1->type);
		zend_ffi_type *type2 = ZEND_FFI_TYPE(cdata2->type);

		if (type1->kind == ZEND_FFI_TYPE_POINTER && type2->kind == ZEND_FFI_TYPE_POINTER) {
			void *ptr1 = *(void**)cdata1->ptr;
			void *ptr2 = *(void**)cdata2->ptr;

			if (!ptr1 || !ptr2) {
				zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
				return 0;
			}
			return ptr1 == ptr2 ? 0 : (ptr1 < ptr2 ? -1 : 1);
		}
	}
	zend_throw_error(zend_ffi_exception_ce, "Comparison of incompatible C types");
	return 0;
}
/* }}} */

static int zend_ffi_cdata_count_elements(zval *object, zend_long *count) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);

	if (type->kind != ZEND_FFI_TYPE_ARRAY) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to count() on non C array");
		return FAILURE;
	} else {
		*count = type->array.length;
		return SUCCESS;
	}
}
/* }}} */

typedef struct _zend_ffi_cdata_iterator {
	zend_object_iterator it;
	zend_long key;
	zval value;
	zend_bool by_ref;
} zend_ffi_cdata_iterator;

static void zend_ffi_cdata_it_dtor(zend_object_iterator *iter) /* {{{ */
{
	zval_ptr_dtor(&((zend_ffi_cdata_iterator*)iter)->value);
	zval_ptr_dtor(&iter->data);
}
/* }}} */

static int zend_ffi_cdata_it_valid(zend_object_iterator *it) /* {{{ */
{
	zend_ffi_cdata_iterator *iter = (zend_ffi_cdata_iterator*)it;
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ(iter->it.data);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);

	return (iter->key >= 0 && iter->key < type->array.length) ? SUCCESS : FAILURE;
}
/* }}} */

static zval *zend_ffi_cdata_it_get_current_data(zend_object_iterator *it) /* {{{ */
{
	zend_ffi_cdata_iterator *iter = (zend_ffi_cdata_iterator*)it;
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ(iter->it.data);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	void *ptr;

	if (!cdata->ptr) {
		zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
		return &EG(uninitialized_zval);
	}
	type = ZEND_FFI_TYPE(type->array.type);
	ptr = (void*)((char*)cdata->ptr + type->size * iter->it.index);

	zval_ptr_dtor(&iter->value);
	if (zend_ffi_cdata_to_zval(NULL, ptr, type, iter->by_ref ? BP_VAR_RW : BP_VAR_R, &iter->value, (cdata->flags & ZEND_FFI_FLAG_CONST) || (type->attr & ZEND_FFI_ATTR_CONST), 0) != SUCCESS) {
		return &EG(uninitialized_zval);
	}
	return &iter->value;
}
/* }}} */

static void zend_ffi_cdata_it_get_current_key(zend_object_iterator *it, zval *key) /* {{{ */
{
	zend_ffi_cdata_iterator *iter = (zend_ffi_cdata_iterator*)it;
	ZVAL_LONG(key, iter->key);
}
/* }}} */

static void zend_ffi_cdata_it_move_forward(zend_object_iterator *it) /* {{{ */
{
	zend_ffi_cdata_iterator *iter = (zend_ffi_cdata_iterator*)it;
	iter->key++;
}
/* }}} */

static void zend_ffi_cdata_it_rewind(zend_object_iterator *it) /* {{{ */
{
	zend_ffi_cdata_iterator *iter = (zend_ffi_cdata_iterator*)it;
	iter->key = 0;
}
/* }}} */

static const zend_object_iterator_funcs zend_ffi_cdata_it_funcs = {
	zend_ffi_cdata_it_dtor,
	zend_ffi_cdata_it_valid,
	zend_ffi_cdata_it_get_current_data,
	zend_ffi_cdata_it_get_current_key,
	zend_ffi_cdata_it_move_forward,
	zend_ffi_cdata_it_rewind,
	NULL
};

static zend_object_iterator *zend_ffi_cdata_get_iterator(zend_class_entry *ce, zval *object, int by_ref) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	zend_ffi_cdata_iterator *iter;

	if (type->kind != ZEND_FFI_TYPE_ARRAY) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to iterate on non C array");
		return NULL;
	}

	iter = emalloc(sizeof(zend_ffi_cdata_iterator));

	zend_iterator_init(&iter->it);

	ZVAL_COPY(&iter->it.data, object);
	iter->it.funcs = &zend_ffi_cdata_it_funcs;
	iter->key = 0;
	iter->by_ref = by_ref;
	ZVAL_UNDEF(&iter->value);

	return &iter->it;
}
/* }}} */

static HashTable *zend_ffi_cdata_get_debug_info(zval *object, int *is_temp) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	void           *ptr = cdata->ptr;
	HashTable      *ht = NULL;
	zend_string    *key;
	zend_ffi_field *f;
	zend_long       n;
	zval            tmp;

	if (!cdata->ptr) {
		zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
		return NULL;
	}

    switch (type->kind) {
		case ZEND_FFI_TYPE_BOOL:
		case ZEND_FFI_TYPE_CHAR:
		case ZEND_FFI_TYPE_ENUM:
		case ZEND_FFI_TYPE_FLOAT:
		case ZEND_FFI_TYPE_DOUBLE:
#ifdef HAVE_LONG_DOUBLE
		case ZEND_FFI_TYPE_LONGDOUBLE:
#endif
		case ZEND_FFI_TYPE_UINT8:
		case ZEND_FFI_TYPE_SINT8:
		case ZEND_FFI_TYPE_UINT16:
		case ZEND_FFI_TYPE_SINT16:
		case ZEND_FFI_TYPE_UINT32:
		case ZEND_FFI_TYPE_SINT32:
		case ZEND_FFI_TYPE_UINT64:
		case ZEND_FFI_TYPE_SINT64:
			if (zend_ffi_cdata_to_zval(cdata, ptr, type, BP_VAR_R, &tmp, 1, 0) == SUCCESS) {
				ht = zend_new_array(1);
				zend_hash_str_add(ht, "cdata", sizeof("cdata")-1, &tmp);
				*is_temp = 1;
				return ht;
			}
			break;
		case ZEND_FFI_TYPE_POINTER:
			if (*(void**)ptr == NULL) {
				ZVAL_NULL(&tmp);
				ht = zend_new_array(1);
				zend_hash_index_add_new(ht, 0, &tmp);
				*is_temp = 1;
				return ht;
			} else if (ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_VOID) {
				ZVAL_LONG(&tmp, (uintptr_t)*(void**)ptr);
				ht = zend_new_array(1);
				zend_hash_index_add_new(ht, 0, &tmp);
				*is_temp = 1;
				return ht;
			} else if (zend_ffi_cdata_to_zval(NULL, *(void**)ptr, ZEND_FFI_TYPE(type->pointer.type), BP_VAR_R, &tmp, 1, 0) == SUCCESS) {
				ht = zend_new_array(1);
				zend_hash_index_add_new(ht, 0, &tmp);
				*is_temp = 1;
				return ht;
			}
			break;
		case ZEND_FFI_TYPE_STRUCT:
			ht = zend_new_array(zend_hash_num_elements(&type->record.fields));
			ZEND_HASH_FOREACH_STR_KEY_PTR(&type->record.fields, key, f) {
				if (key) {
					if (!f->bits) {
						void *f_ptr = (void*)(((char*)ptr) + f->offset);
						if (zend_ffi_cdata_to_zval(NULL, f_ptr, ZEND_FFI_TYPE(f->type), BP_VAR_R, &tmp, 1, 0) == SUCCESS) {
							zend_hash_add(ht, key, &tmp);
						}
					} else {
						zend_ffi_bit_field_to_zval(ptr, f, &tmp);
						zend_hash_add(ht, key, &tmp);
					}
				}
			} ZEND_HASH_FOREACH_END();
			*is_temp = 1;
			return ht;
		case ZEND_FFI_TYPE_ARRAY:
			ht = zend_new_array(type->array.length);
			for (n = 0; n < type->array.length; n++) {
				if (zend_ffi_cdata_to_zval(NULL, ptr, ZEND_FFI_TYPE(type->array.type), BP_VAR_R, &tmp, 1, 0) == SUCCESS) {
					zend_hash_index_add(ht, n, &tmp);
				}
				ptr = (void*)(((char*)ptr) + ZEND_FFI_TYPE(type->array.type)->size);
			}
			*is_temp = 1;
			return ht;
		case ZEND_FFI_TYPE_FUNC:
			ht = zend_new_array(0);
			// TODO: function name ???
			*is_temp = 1;
			return ht;
			break;
		default:
			ZEND_ASSERT(0);
			break;
	}
	return NULL;
}
/* }}} */

static int zend_ffi_cdata_get_closure(zval *obj, zend_class_entry **ce_ptr, zend_function **fptr_ptr, zend_object **obj_ptr) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(obj);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	zend_function  *func;

	if (!cdata->ptr) {
		zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
		return FAILURE;
	}

	if (type->kind != ZEND_FFI_TYPE_POINTER) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to call non C function pointer");
		return FAILURE;
	}
	type = ZEND_FFI_TYPE(type->pointer.type);
	if (type->kind != ZEND_FFI_TYPE_FUNC) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to call non C function pointer");
		return FAILURE;
	}

	if (EXPECTED(EG(trampoline).common.function_name == NULL)) {
		func = &EG(trampoline);
	} else {
		func = ecalloc(sizeof(zend_internal_function), 1);
	}
	func->type = ZEND_INTERNAL_FUNCTION;
	func->common.arg_flags[0] = 0;
	func->common.arg_flags[1] = 0;
	func->common.arg_flags[2] = 0;
	func->common.fn_flags = ZEND_ACC_CALL_VIA_TRAMPOLINE;
	func->common.function_name = ZSTR_KNOWN(ZEND_STR_MAGIC_INVOKE);
	func->common.num_args = func->common.required_num_args = type->func.args ? zend_hash_num_elements(type->func.args) : 0;
	func->internal_function.handler = ZEND_FN(ffi_trampoline);

	if (func->common.num_args > MAX_ARG_FLAG_NUM) {
		func->common.arg_info = emalloc(sizeof(zend_arg_info) * func->common.num_args);
		memset(func->common.arg_info, 0, sizeof(zend_arg_info) * func->common.num_args);
	}

	func->internal_function.reserved[0] = type;
	func->internal_function.reserved[1] = *(void**)cdata->ptr;

	*ce_ptr = NULL;
	*fptr_ptr= func;
	*obj_ptr = NULL;

	return SUCCESS;
}
/* }}} */

static zend_object *zend_ffi_ctype_new(zend_class_entry *class_type) /* {{{ */
{
	zend_ffi_ctype *ctype;

	ctype = emalloc(sizeof(zend_ffi_ctype));
	memset(ctype, 0, sizeof(zend_ffi_ctype));

	zend_object_std_init(&ctype->std, class_type);
	ctype->std.handlers = &zend_ffi_ctype_handlers;

	ctype->type = NULL;

	return &ctype->std;
}
/* }}} */

static void zend_ffi_ctype_free_obj(zend_object *object) /* {{{ */
{
	zend_ffi_ctype *ctype = (zend_ffi_ctype*)object;

	zend_object_std_dtor(&ctype->std);
	zend_ffi_type_dtor(ctype->type);
}
/* }}} */

static int zend_ffi_is_same_type(zend_ffi_type *type1, zend_ffi_type *type2) /* {{{ */
{
	while (1) {
		if (type1 == type2) {
			return 1;
		} else if (type1->kind == type2->kind) {
			if (type1->kind < ZEND_FFI_TYPE_POINTER) {
				return 1;
			} else if (type1->kind == ZEND_FFI_TYPE_POINTER) {
				type1 = ZEND_FFI_TYPE(type1->pointer.type);
				type2 = ZEND_FFI_TYPE(type2->pointer.type);
				if (type1->kind == ZEND_FFI_TYPE_VOID ||
				    type2->kind == ZEND_FFI_TYPE_VOID) {
				    return 1;
				}
			} else if (type1->kind == ZEND_FFI_TYPE_ARRAY &&
			           type1->array.length == type2->array.length) {
				type1 = ZEND_FFI_TYPE(type1->array.type);
				type2 = ZEND_FFI_TYPE(type2->array.type);
			} else {
				break;
			}
		} else {
			break;
		}
	}
	return 0;
}
/* }}} */

static zend_string *zend_ffi_ctype_get_class_name(const zend_object *zobj) /* {{{ */
{
	zend_ffi_ctype *ctype = (zend_ffi_ctype*)zobj;

	return zend_ffi_get_class_name(zobj->ce->name, ZEND_FFI_TYPE(ctype->type));
}
/* }}} */

static int zend_ffi_ctype_compare_objects(zval *o1, zval *o2) /* {{{ */
{
	if (Z_TYPE_P(o1) == IS_OBJECT && Z_OBJCE_P(o1) == zend_ffi_ctype_ce &&
	    Z_TYPE_P(o2) == IS_OBJECT && Z_OBJCE_P(o2) == zend_ffi_ctype_ce) {
		zend_ffi_ctype *ctype1 = (zend_ffi_ctype*)Z_OBJ_P(o1);
		zend_ffi_ctype *ctype2 = (zend_ffi_ctype*)Z_OBJ_P(o2);
		zend_ffi_type *type1 = ZEND_FFI_TYPE(ctype1->type);
		zend_ffi_type *type2 = ZEND_FFI_TYPE(ctype2->type);

		if (zend_ffi_is_same_type(type1, type2)) {
			return 0;
		} else {
			return 1;
		}
	}
	zend_throw_error(zend_ffi_exception_ce, "Comparison of incompatible C types");
	return 0;
}
/* }}} */

static HashTable *zend_ffi_ctype_get_debug_info(zval *object, int *is_temp) /* {{{ */
{
	return NULL;
}
/* }}} */

static zend_object *zend_ffi_new(zend_class_entry *class_type) /* {{{ */
{
	zend_ffi *ffi;

	ffi = emalloc(sizeof(zend_ffi));
	memset(ffi, 0, sizeof(zend_ffi));

	zend_object_std_init(&ffi->std, class_type);
	ffi->std.handlers = &zend_ffi_handlers;

	ffi->lib = NULL;

	return &ffi->std;
}
/* }}} */

static void zend_ffi_type_dtor(zend_ffi_type *type) /* {{{ */
{
	if (!ZEND_FFI_TYPE_IS_OWNED(type)) {
		return;
	}
	type = ZEND_FFI_TYPE(type);

	switch (type->kind) {
		case ZEND_FFI_TYPE_STRUCT:
			zend_hash_destroy(&type->record.fields);
			break;
		case ZEND_FFI_TYPE_POINTER:
			zend_ffi_type_dtor(type->pointer.type);
			break;
		case ZEND_FFI_TYPE_ARRAY:
			zend_ffi_type_dtor(type->array.type);
			break;
		case ZEND_FFI_TYPE_FUNC:
			if (type->func.args) {
				zend_hash_destroy(type->func.args);
				pefree(type->func.args, type->attr & ZEND_FFI_ATTR_PERSISTENT);
			}
			zend_ffi_type_dtor(type->func.ret_type);
			break;
		default:
			break;
	}
	pefree(type, type->attr & ZEND_FFI_ATTR_PERSISTENT);
}
/* }}} */

static void zend_ffi_type_hash_dtor(zval *zv) /* {{{ */
{
	zend_ffi_type *type = Z_PTR_P(zv);
	zend_ffi_type_dtor(type);
}
/* }}} */

static void zend_ffi_field_hash_dtor(zval *zv) /* {{{ */
{
	zend_ffi_field *field = Z_PTR_P(zv);
	zend_ffi_type_dtor(field->type);
	efree(field);
}
/* }}} */

static void zend_ffi_field_hash_persistent_dtor(zval *zv) /* {{{ */
{
	zend_ffi_field *field = Z_PTR_P(zv);
	zend_ffi_type_dtor(field->type);
	free(field);
}
/* }}} */

static void zend_ffi_symbol_hash_dtor(zval *zv) /* {{{ */
{
	zend_ffi_symbol *sym = Z_PTR_P(zv);
	zend_ffi_type_dtor(sym->type);
	efree(sym);
}
/* }}} */

static void zend_ffi_symbol_hash_persistent_dtor(zval *zv) /* {{{ */
{
	zend_ffi_symbol *sym = Z_PTR_P(zv);
	zend_ffi_type_dtor(sym->type);
	free(sym);
}
/* }}} */

static void zend_ffi_tag_hash_dtor(zval *zv) /* {{{ */
{
	zend_ffi_tag *tag = Z_PTR_P(zv);
	zend_ffi_type_dtor(tag->type);
	efree(tag);
}
/* }}} */

static void zend_ffi_tag_hash_persistent_dtor(zval *zv) /* {{{ */
{
	zend_ffi_tag *tag = Z_PTR_P(zv);
	zend_ffi_type_dtor(tag->type);
	free(tag);
}
/* }}} */

static void zend_ffi_cdata_dtor(zend_ffi_cdata *cdata) /* {{{ */
{
	zend_ffi_type_dtor(cdata->type);
	if (cdata->flags & ZEND_FFI_FLAG_OWNED) {
		pefree(cdata->ptr, cdata->flags & ZEND_FFI_FLAG_PERSISTENT);
	}
}
/* }}} */

static void zend_ffi_scope_hash_dtor(zval *zv) /* {{{ */
{
	zend_ffi_scope *scope = Z_PTR_P(zv);
	if (scope->symbols) {
		zend_hash_destroy(scope->symbols);
		free(scope->symbols);
	}
	if (scope->tags) {
		zend_hash_destroy(scope->tags);
		free(scope->tags);
	}
	free(scope);
}
/* }}} */

static void zend_ffi_free_obj(zend_object *object) /* {{{ */
{
	zend_ffi *ffi = (zend_ffi*)object;

	zend_object_std_dtor(&ffi->std);

	if (ffi->persistent) {
		return;
	}

	if (ffi->lib) {
		DL_UNLOAD(ffi->lib);
		ffi->lib = NULL;
	}

	if (ffi->symbols) {
		zend_hash_destroy(ffi->symbols);
		efree(ffi->symbols);
	}

	if (ffi->tags) {
		zend_hash_destroy(ffi->tags);
		efree(ffi->tags);
	}
}
/* }}} */

static void zend_ffi_cdata_free_obj(zend_object *object) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)object;

	zend_object_std_dtor(&cdata->std);
	zend_ffi_cdata_dtor(cdata);
}
/* }}} */

static zend_object *zend_ffi_cdata_clone_obj(zval *zobject) /* {{{ */
{
	zend_ffi_cdata *old_cdata = (zend_ffi_cdata*)Z_OBJ_P(zobject);
	zend_ffi_type *type = ZEND_FFI_TYPE(old_cdata->type);
	zend_ffi_cdata *new_cdata;

	new_cdata = (zend_ffi_cdata*)zend_ffi_cdata_new(zend_ffi_cdata_ce);
	if (type->kind < ZEND_FFI_TYPE_POINTER) {
		new_cdata->std.handlers = &zend_ffi_cdata_value_handlers;
	}
	new_cdata->type = type;
	new_cdata->ptr = emalloc(type->size);
	memcpy(new_cdata->ptr, old_cdata->ptr, type->size);
	new_cdata->flags |= ZEND_FFI_FLAG_OWNED;

	return &new_cdata->std;
}
/* }}} */

static zval *zend_ffi_read_var(zval *object, zval *member, int read_type, void **cache_slot, zval *rv) /* {{{ */
{
	zend_ffi        *ffi = (zend_ffi*)Z_OBJ_P(object);
	zend_string     *tmp_var_name;
	zend_string     *var_name = zval_get_tmp_string(member, &tmp_var_name);
	zend_ffi_symbol *sym = NULL;

	if (ffi->symbols) {
		sym = zend_hash_find_ptr(ffi->symbols, var_name);
		if (sym && sym->kind != ZEND_FFI_SYM_VAR && sym->kind != ZEND_FFI_SYM_CONST) {
			sym = NULL;
		}
	}
	if (!sym) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to read undefined C variable '%s'", ZSTR_VAL(var_name));
		zend_tmp_string_release(tmp_var_name);
		return &EG(uninitialized_zval);
	}

	zend_tmp_string_release(tmp_var_name);

	if (sym->kind == ZEND_FFI_SYM_VAR) {
		if (zend_ffi_cdata_to_zval(NULL, sym->addr, ZEND_FFI_TYPE(sym->type), read_type, rv, sym->is_const, 0) != SUCCESS) {
			return &EG(uninitialized_zval);
		}
	} else {
		ZVAL_LONG(rv, sym->value);
	}

	return rv;
}
/* }}} */

static void zend_ffi_write_var(zval *object, zval *member, zval *value, void **cache_slot) /* {{{ */
{
	zend_ffi        *ffi = (zend_ffi*)Z_OBJ_P(object);
	zend_string     *tmp_var_name;
	zend_string     *var_name = zval_get_tmp_string(member, &tmp_var_name);
	zend_ffi_symbol *sym = NULL;

	if (ffi->symbols) {
		sym = zend_hash_find_ptr(ffi->symbols, var_name);
		if (sym && sym->kind != ZEND_FFI_SYM_VAR) {
			sym = NULL;
		}
	}
	if (!sym) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to assign undefined C variable '%s'", ZSTR_VAL(var_name));
		zend_tmp_string_release(tmp_var_name);
		return;
	}

	zend_tmp_string_release(tmp_var_name);

	if (sym->is_const) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to assign read-only C variable '%s'", ZSTR_VAL(var_name));
		return;
	}

	zend_ffi_zval_to_cdata(sym->addr, ZEND_FFI_TYPE(sym->type), value);
}
/* }}} */

static int zend_ffi_pass_arg(zval *arg, zend_ffi_type *type, ffi_type **pass_type, void **arg_values, uint32_t n) /* {{{ */
{
	zend_long lval;
	double dval;
	zend_string *str, *tmp_str;
	zend_ffi_type_kind kind = type->kind;

	ZVAL_DEREF(arg);

again:
    switch (kind) {
		case ZEND_FFI_TYPE_FLOAT:
			dval = zval_get_double(arg);
			*pass_type = &ffi_type_float;
			*(float*)arg_values[n] = (float)dval;
			break;
		case ZEND_FFI_TYPE_DOUBLE:
			dval = zval_get_double(arg);
			*pass_type = &ffi_type_double;
			*(double*)arg_values[n] = dval;
			break;
#ifdef HAVE_LONG_DOUBLE
		case ZEND_FFI_TYPE_LONGDOUBLE:
			dval = zval_get_double(arg);
			*pass_type = &ffi_type_double;
			*(long double*)arg_values[n] = (long double)dval;
			break;
#endif
		case ZEND_FFI_TYPE_UINT8:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_uint8;
			*(uint8_t*)arg_values[n] = (uint8_t)lval;
			break;
		case ZEND_FFI_TYPE_SINT8:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_sint8;
			*(int8_t*)arg_values[n] = (int8_t)lval;
			break;
		case ZEND_FFI_TYPE_UINT16:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_uint16;
			*(uint16_t*)arg_values[n] = (uint16_t)lval;
			break;
		case ZEND_FFI_TYPE_SINT16:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_sint16;
			*(int16_t*)arg_values[n] = (int16_t)lval;
			break;
		case ZEND_FFI_TYPE_UINT32:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_uint32;
			*(uint32_t*)arg_values[n] = (uint32_t)lval;
			break;
		case ZEND_FFI_TYPE_SINT32:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_sint32;
			*(int32_t*)arg_values[n] = (int32_t)lval;
			break;
		case ZEND_FFI_TYPE_UINT64:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_uint64;
			*(uint64_t*)arg_values[n] = (uint64_t)lval;
			break;
		case ZEND_FFI_TYPE_SINT64:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_sint64;
			*(int64_t*)arg_values[n] = (int64_t)lval;
			break;
		case ZEND_FFI_TYPE_POINTER:
			*pass_type = &ffi_type_pointer;
			if (Z_TYPE_P(arg) == IS_NULL) {
				*(void**)arg_values[n] = NULL;
				return SUCCESS;
			} else if (Z_TYPE_P(arg) == IS_STRING
			        && ((ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_CHAR)
			         || (ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_VOID))) {
				*(void**)arg_values[n] = Z_STRVAL_P(arg);
				return SUCCESS;
			} else if (Z_TYPE_P(arg) == IS_OBJECT && Z_OBJCE_P(arg) == zend_ffi_cdata_ce) {
				zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(arg);

				if (zend_ffi_is_compatible_type(type, ZEND_FFI_TYPE(cdata->type))) {
					if (ZEND_FFI_TYPE(cdata->type)->kind == ZEND_FFI_TYPE_POINTER) {
						if (!cdata->ptr) {
							zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
							return FAILURE;
						}
						*(void**)arg_values[n] = *(void**)cdata->ptr;
					} else {
						*(void**)arg_values[n] = cdata->ptr;
					}
					return SUCCESS;
				}
#if FFI_CLOSURES
			} else if (ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_FUNC) {
				void *callback = zend_ffi_create_callback(ZEND_FFI_TYPE(type->pointer.type), arg);

				if (callback) {
					*(void**)arg_values[n] = callback;
					break;
				} else {
					return FAILURE;
				}
#endif
			}
			zend_throw_error(zend_ffi_exception_ce, "Passing incompatible pointer");
			return FAILURE;
		case ZEND_FFI_TYPE_BOOL:
			*pass_type = &ffi_type_uint8;
			*(uint8_t*)arg_values[n] = zend_is_true(arg);
			break;
		case ZEND_FFI_TYPE_CHAR:
			str = zval_get_tmp_string(arg, &tmp_str);
			*pass_type = &ffi_type_sint8;
			*(char*)arg_values[n] = ZSTR_VAL(str)[0];
			if (ZSTR_LEN(str) != 1) {
				zend_throw_error(zend_ffi_exception_ce, "Attempt to pass incompatible C type");
			}
			zend_tmp_string_release(tmp_str);
			break;
		case ZEND_FFI_TYPE_ENUM:
			kind = type->enumeration.kind;
			goto again;
		case ZEND_FFI_TYPE_STRUCT:
			if (!(type->attr & ZEND_FFI_ATTR_UNION)
			 && Z_TYPE_P(arg) == IS_OBJECT && Z_OBJCE_P(arg) == zend_ffi_cdata_ce) {
				zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(arg);

				if (zend_ffi_is_compatible_type(type, ZEND_FFI_TYPE(cdata->type))) {
				    /* Create a fake structure type */
					ffi_type *t = zend_ffi_make_fake_struct_type(type);
					if (!t) {
						return FAILURE;
					}
					*pass_type = t;
					arg_values[n] = cdata->ptr;
					break;
				} else {
					zend_throw_error(zend_ffi_exception_ce, "Passing incompatible struct/union");
					return FAILURE;
				}
			}
			zend_throw_error(zend_ffi_exception_ce, "FFI passing struct/union is not implemented");
			return FAILURE;
		case ZEND_FFI_TYPE_ARRAY:
			zend_throw_error(zend_ffi_exception_ce, "FFI passing array is not implemented");
			return FAILURE;
		default:
			zend_throw_error(zend_ffi_exception_ce, "FFI internal error");
			return FAILURE;
	}
	return SUCCESS;
}
/* }}} */

static int zend_ffi_pass_var_arg(zval *arg, ffi_type **pass_type, void **pass_val) /* {{{ */
{
	ZVAL_DEREF(arg);
	switch (Z_TYPE_P(arg)) {
		case IS_NULL:
			*pass_type = &ffi_type_pointer;
			*(void**)pass_val = NULL;
			break;
		case IS_FALSE:
			*pass_type = &ffi_type_uint8;
			*(uint8_t*)pass_val = 0;
			break;
		case IS_TRUE:
			*pass_type = &ffi_type_uint8;
			*(uint8_t*)pass_val = 1;
			break;
		case IS_LONG:
			if (sizeof(zend_long) == 4) {
				*pass_type = &ffi_type_sint32;
				*(int32_t*)pass_val = Z_LVAL_P(arg);
			} else {
				*pass_type = &ffi_type_sint64;
				*(int64_t*)pass_val = Z_LVAL_P(arg);
			}
			break;
		case IS_DOUBLE:
			*pass_type = &ffi_type_double;
			*(double*)pass_val = Z_DVAL_P(arg);
			break;
		case IS_STRING:
			*pass_type = &ffi_type_pointer;
			*(char**)pass_val = Z_STRVAL_P(arg);
			break;
		default:
			zend_throw_error(zend_ffi_exception_ce, "FFI internal error");
			return FAILURE;
	}
	return SUCCESS;
}
/* }}} */

static ZEND_FUNCTION(ffi_trampoline) /* {{{ */
{
	zend_ffi_type *type = EX(func)->internal_function.reserved[0];
	void *addr = EX(func)->internal_function.reserved[1];
	ffi_cif cif;
	ffi_type *ret_type = NULL;
	ffi_type **arg_types = NULL;
	void **arg_values = NULL;
	uint32_t n, arg_count;
	ffi_arg ret;
	zend_ffi_type *arg_type;
	ALLOCA_FLAG(arg_types_use_heap = 0)
	ALLOCA_FLAG(arg_values_use_heap = 0)

	ZEND_ASSERT(type->kind == ZEND_FFI_TYPE_FUNC);
	arg_count = type->func.args ? zend_hash_num_elements(type->func.args) : 0;
	if (type->attr & ZEND_FFI_ATTR_VARIADIC) {
		if (arg_count > EX_NUM_ARGS()) {
			zend_throw_error(zend_ffi_exception_ce, "Incorrect number of arguments for C function '%s'", ZSTR_VAL(EX(func)->internal_function.function_name));
			return;
		}
		if (EX_NUM_ARGS()) {
			arg_types = do_alloca(
				sizeof(ffi_type*) * EX_NUM_ARGS(), arg_types_use_heap);
			arg_values = do_alloca(
				(sizeof(void*) + FFI_SIZEOF_ARG) * EX_NUM_ARGS(), arg_values_use_heap);
			n = 0;
			if (type->func.args) {
				ZEND_HASH_FOREACH_PTR(type->func.args, arg_type) {
					arg_type = ZEND_FFI_TYPE(arg_type);
					arg_values[n] = ((char*)arg_values) + (sizeof(void*) * EX_NUM_ARGS()) + (FFI_SIZEOF_ARG * n);
					if (zend_ffi_pass_arg(EX_VAR_NUM(n), arg_type, &arg_types[n], arg_values, n) != SUCCESS) {
						free_alloca(arg_types, arg_types_use_heap);
						free_alloca(arg_values, arg_values_use_heap);
						return;
					}
					n++;
				} ZEND_HASH_FOREACH_END();
			}
			for (; n < EX_NUM_ARGS(); n++) {
				arg_values[n] = ((char*)arg_values) + (sizeof(void*) * EX_NUM_ARGS()) + (FFI_SIZEOF_ARG * n);
				if (zend_ffi_pass_var_arg(EX_VAR_NUM(n), &arg_types[n], arg_values[n]) != SUCCESS) {
					free_alloca(arg_types, arg_types_use_heap);
					free_alloca(arg_values, arg_values_use_heap);
					return;
				}
			}
		}
		ret_type = zend_ffi_get_type(ZEND_FFI_TYPE(type->func.ret_type));
		if (!ret_type) {
			free_alloca(arg_types, arg_types_use_heap);
			free_alloca(arg_values, arg_values_use_heap);
			return;
		}
		if (ffi_prep_cif_var(&cif, type->func.abi, arg_count, EX_NUM_ARGS(), ret_type, arg_types) != FFI_OK) {
			zend_throw_error(zend_ffi_exception_ce, "FFI internal error");
			free_alloca(arg_types, arg_types_use_heap);
			free_alloca(arg_values, arg_values_use_heap);
			return;
		}
	} else {
		if (arg_count != EX_NUM_ARGS()) {
			zend_throw_error(zend_ffi_exception_ce, "Incorrect number of arguments for C function '%s'", ZSTR_VAL(EX(func)->internal_function.function_name));
			return;
		}
		if (EX_NUM_ARGS()) {
			arg_types = do_alloca(
				(sizeof(ffi_type*) + sizeof(ffi_type)) * EX_NUM_ARGS(), arg_types_use_heap);
			arg_values = do_alloca(
				(sizeof(void*) + FFI_SIZEOF_ARG) * EX_NUM_ARGS(), arg_values_use_heap);
			n = 0;
			if (type->func.args) {
				ZEND_HASH_FOREACH_PTR(type->func.args, arg_type) {
					arg_type = ZEND_FFI_TYPE(arg_type);
					arg_values[n] = ((char*)arg_values) + (sizeof(void*) * EX_NUM_ARGS()) + (FFI_SIZEOF_ARG * n);
					if (zend_ffi_pass_arg(EX_VAR_NUM(n), arg_type, &arg_types[n], arg_values, n) != SUCCESS) {
						free_alloca(arg_types, arg_types_use_heap);
						free_alloca(arg_values, arg_values_use_heap);
						return;
					}
					n++;
				} ZEND_HASH_FOREACH_END();
			}
		}
		ret_type = zend_ffi_get_type(ZEND_FFI_TYPE(type->func.ret_type));
		if (!ret_type) {
			free_alloca(arg_types, arg_types_use_heap);
			free_alloca(arg_values, arg_values_use_heap);
			return;
		}
		if (ffi_prep_cif(&cif, type->func.abi, arg_count, ret_type, arg_types) != FFI_OK) {
			zend_throw_error(zend_ffi_exception_ce, "FFI internal error");
			free_alloca(arg_types, arg_types_use_heap);
			free_alloca(arg_values, arg_values_use_heap);
			return;
		}
	}

	ffi_call(&cif, addr, &ret, arg_values);

	for (n = 0; n < arg_count; n++) {
		if (arg_types[n]->type == FFI_TYPE_STRUCT) {
			efree(arg_types[n]);
		}
	}
	if (ret_type->type == FFI_TYPE_STRUCT) {
		efree(ret_type);
	}

	if (EX_NUM_ARGS()) {
		free_alloca(arg_types, arg_types_use_heap);
		free_alloca(arg_values, arg_values_use_heap);
	}

	zend_ffi_cdata_to_zval(NULL, (void*)&ret, ZEND_FFI_TYPE(type->func.ret_type), BP_VAR_R, return_value, 0, 1);

	zend_string_release(EX(func)->common.function_name);
	if (EX(func)->common.fn_flags & ZEND_ACC_CALL_VIA_TRAMPOLINE) {
		if (EX(func)->common.arg_info) {
			efree(EX(func)->common.arg_info);
		}
		zend_free_trampoline(EX(func));
		EX(func) = NULL;
	}
}
/* }}} */

static zend_function *zend_ffi_get_func(zend_object **obj, zend_string *name, const zval *key) /* {{{ */
{
	zend_ffi        *ffi = (zend_ffi*)*obj;
	zend_ffi_symbol *sym = NULL;
	zend_function   *func;
	zend_ffi_type   *type;

	if (ZSTR_LEN(name) == sizeof("new") -1
	 && (ZSTR_VAL(name)[0] == 'n' || ZSTR_VAL(name)[0] == 'N')
	 && (ZSTR_VAL(name)[1] == 'e' || ZSTR_VAL(name)[1] == 'E')
	 && (ZSTR_VAL(name)[2] == 'w' || ZSTR_VAL(name)[2] == 'W')) {
		return (zend_function*)&zend_ffi_new_fn;
	} else if (ZSTR_LEN(name) == sizeof("cast") -1
	 && (ZSTR_VAL(name)[0] == 'c' || ZSTR_VAL(name)[0] == 'C')
	 && (ZSTR_VAL(name)[1] == 'a' || ZSTR_VAL(name)[1] == 'A')
	 && (ZSTR_VAL(name)[2] == 's' || ZSTR_VAL(name)[2] == 'S')
	 && (ZSTR_VAL(name)[3] == 't' || ZSTR_VAL(name)[3] == 'T')) {
		return (zend_function*)&zend_ffi_cast_fn;
	} else if (ZSTR_LEN(name) == sizeof("type") -1
	 && (ZSTR_VAL(name)[0] == 't' || ZSTR_VAL(name)[0] == 'T')
	 && (ZSTR_VAL(name)[1] == 'y' || ZSTR_VAL(name)[1] == 'Y')
	 && (ZSTR_VAL(name)[2] == 'p' || ZSTR_VAL(name)[2] == 'P')
	 && (ZSTR_VAL(name)[3] == 'e' || ZSTR_VAL(name)[3] == 'E')) {
		return (zend_function*)&zend_ffi_type_fn;
	}

	if (ffi->symbols) {
		sym = zend_hash_find_ptr(ffi->symbols, name);
		if (sym && sym->kind != ZEND_FFI_SYM_FUNC) {
			sym = NULL;
		}
	}
	if (!sym) {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to call undefined C function '%s'", ZSTR_VAL(name));
		return NULL;
	}

	type = ZEND_FFI_TYPE(sym->type);
	ZEND_ASSERT(type->kind == ZEND_FFI_TYPE_FUNC);

	if (EXPECTED(EG(trampoline).common.function_name == NULL)) {
		func = &EG(trampoline);
	} else {
		func = ecalloc(sizeof(zend_internal_function), 1);
	}
	func->common.type = ZEND_INTERNAL_FUNCTION;
	func->common.arg_flags[0] = 0;
	func->common.arg_flags[1] = 0;
	func->common.arg_flags[2] = 0;
	func->common.fn_flags = ZEND_ACC_CALL_VIA_TRAMPOLINE;
	func->common.function_name = zend_string_copy(name);
	func->common.num_args = func->common.required_num_args = type->func.args ? zend_hash_num_elements(type->func.args) : 0;
	func->internal_function.handler = ZEND_FN(ffi_trampoline);

	if (func->common.num_args > MAX_ARG_FLAG_NUM) {
		func->common.arg_info = emalloc(sizeof(zend_arg_info) * func->common.num_args);
		memset(func->common.arg_info, 0, sizeof(zend_arg_info) * func->common.num_args);
	}

	func->internal_function.reserved[0] = type;
	func->internal_function.reserved[1] = sym->addr;

	return func;
}
/* }}} */

ZEND_METHOD(FFI, __construct) /* {{{ */
{
	zend_string *code = NULL;
	zend_string *lib = NULL;
	zend_ffi *ffi = (zend_ffi*)Z_OBJ(EX(This));
	void *addr;

	ZEND_PARSE_PARAMETERS_START(0, 2)
		Z_PARAM_OPTIONAL
		Z_PARAM_STR(code)
		Z_PARAM_STR(lib)
	ZEND_PARSE_PARAMETERS_END();

	if (lib) {
		DL_HANDLE handle = DL_LOAD(ZSTR_VAL(lib));
		if (!handle) {
			zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s'", ZSTR_VAL(lib));
			return;
		}
		ffi->lib = handle;
#ifdef RTLD_DEFAULT
	} else if (1) {
		// TODO: this might need to be disabled or protected ???
		ffi->lib = RTLD_DEFAULT;
#endif
	}

	if (code) {
		/* Parse C definitions */
		FFI_G(symbols) = NULL;
		FFI_G(tags) = NULL;

		if (zend_ffi_parse_decl(ZSTR_VAL(code), ZSTR_LEN(code)) != SUCCESS) {
			if (FFI_G(symbols)) {
				zend_hash_destroy(FFI_G(symbols));
				efree(FFI_G(symbols));
				FFI_G(symbols) = NULL;
			}
			if (FFI_G(tags)) {
				zend_hash_destroy(FFI_G(tags));
				efree(FFI_G(tags));
				FFI_G(tags) = NULL;
			}
			return;
		}
		ffi->symbols = FFI_G(symbols);
		ffi->tags = FFI_G(tags);
		FFI_G(symbols) = NULL;
		FFI_G(tags) = NULL;

		if (ffi->symbols) {
			zend_string *name;
			zend_ffi_symbol *sym;

			ZEND_HASH_FOREACH_STR_KEY_PTR(ffi->symbols, name, sym) {
				if (sym->kind == ZEND_FFI_SYM_VAR) {
					addr = DL_FETCH_SYMBOL(ffi->lib, ZSTR_VAL(name));
					if (!addr) {
						zend_throw_error(zend_ffi_exception_ce, "Failed resolving C variable '%s'", ZSTR_VAL(name));
					}
					sym->addr = addr;
				} else if (sym->kind == ZEND_FFI_SYM_FUNC) {
					addr = DL_FETCH_SYMBOL(ffi->lib, ZSTR_VAL(name));
					if (!addr) {
						zend_throw_error(zend_ffi_exception_ce, "Failed resolving C function '%s'", ZSTR_VAL(name));
					}
					sym->addr = addr;
				}
			} ZEND_HASH_FOREACH_END();
		}
	}
}
/* }}} */

ZEND_METHOD(FFI, load) /* {{{ */
{
	zend_string *fn;
	struct stat buf;
	int fd;
	char *filename, *code, *code_pos, *scope_name, *lib;
	size_t code_size;
	zend_ffi *ffi;

	ZEND_PARSE_PARAMETERS_START(1, 1)
		Z_PARAM_STR(fn)
	ZEND_PARSE_PARAMETERS_END();

	filename = ZSTR_VAL(fn);
	if (stat(filename, &buf) != 0) {
		zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s', file doesn't exist", filename);
		return;
	}

	if ((buf.st_mode & S_IFMT) != S_IFREG) {
		zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s', not a regular file", filename);
		return;
	}

	code_size = buf.st_size;
	code = emalloc(code_size + 1);
	fd = open(filename, O_RDONLY, 0);
	if (fd < 0 || read(fd, code, code_size) != code_size) {
		zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s', cannot read_file", filename);
		efree(code);
		close(fd);
		return;
	}
	close(fd);
	code[code_size] = 0;

	FFI_G(symbols) = NULL;
	FFI_G(tags) = NULL;

	scope_name = NULL;
	lib = NULL;
	code_pos = zend_ffi_parse_directives(filename, code, &scope_name, &lib, 0);
	if (!code_pos) {
		efree(code);
		return;
	}
	code_size -= code_pos - code;

	if (zend_ffi_parse_decl(code_pos, code_size) != SUCCESS) {
		zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s'", filename);
		efree(code);
		goto cleanup;
	}

	ffi = (zend_ffi*)zend_ffi_new(zend_ffi_ce);

	if (lib) {
		DL_HANDLE handle = DL_LOAD(lib);
		if (!handle) {
			zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s'", lib);
			efree(code);
			goto cleanup;
		}
		ffi->lib = handle;
#ifdef RTLD_DEFAULT
	} else if (1) {
		// TODO: this might need to be disabled or protected ???
		ffi->lib = RTLD_DEFAULT;
#endif
	}

	efree(code);
	ffi->symbols = FFI_G(symbols);
	ffi->tags = FFI_G(tags);

	if (ffi->symbols) {
		zend_string *name;
		zend_ffi_symbol *sym;
		void *addr;

		ZEND_HASH_FOREACH_STR_KEY_PTR(ffi->symbols, name, sym) {
			if (sym->kind == ZEND_FFI_SYM_VAR) {
				addr = DL_FETCH_SYMBOL(ffi->lib, ZSTR_VAL(name));
				if (!addr) {
					zend_throw_error(zend_ffi_exception_ce, "Failed resolving C variable '%s'", ZSTR_VAL(name));
					goto cleanup;
				}
				sym->addr = addr;
			} else if (sym->kind == ZEND_FFI_SYM_FUNC) {
				addr = DL_FETCH_SYMBOL(ffi->lib, ZSTR_VAL(name));
				if (!addr) {
					zend_throw_error(zend_ffi_exception_ce, "Failed resolving C function '%s'", ZSTR_VAL(name));
					goto cleanup;
				}
				sym->addr = addr;
			}
		} ZEND_HASH_FOREACH_END();
	}

	FFI_G(symbols) = NULL;
	FFI_G(tags) = NULL;

	RETURN_OBJ(&ffi->std);

cleanup:
	if (FFI_G(symbols)) {
		zend_hash_destroy(FFI_G(symbols));
		efree(FFI_G(symbols));
		FFI_G(symbols) = NULL;
	}
	if (FFI_G(tags)) {
		zend_hash_destroy(FFI_G(tags));
		efree(FFI_G(tags));
		FFI_G(tags) = NULL;
	}
}
/* }}} */

ZEND_METHOD(FFI, scope) /* {{{ */
{
	zend_string *scope_name;
	zend_ffi_scope *scope = NULL;
	zend_ffi *ffi;

	ZEND_PARSE_PARAMETERS_START(1, 1)
		Z_PARAM_STR(scope_name)
	ZEND_PARSE_PARAMETERS_END();

	if (FFI_G(scopes)) {
		scope = zend_hash_find_ptr(FFI_G(scopes), scope_name);
	}

	if (!scope) {
		zend_throw_error(zend_ffi_exception_ce, "Failed loading scope '%s'", ZSTR_VAL(scope_name));
		return;
	}

	ffi = (zend_ffi*)zend_ffi_new(zend_ffi_ce);

	ffi->symbols = scope->symbols;
	ffi->tags = scope->tags;
	ffi->persistent = 1;

	RETURN_OBJ(&ffi->std);
}
/* }}} */

static void zend_ffi_cleanup_dcl(zend_ffi_dcl *dcl) /* {{{ */
{
	if (dcl) {
		zend_ffi_type_dtor(dcl->type);
		dcl->type = NULL;
	}
}
/* }}} */

static void zend_ffi_throw_parser_error(const char *format, ...) /* {{{ */
{
	va_list va;
	char *message = NULL;

	va_start(va, format);
	zend_vspprintf(&message, 0, format, va);

	if (EG(current_execute_data)) {
		zend_throw_exception(zend_ffi_parser_exception_ce, message, 0);
	} else {
		zend_error(E_WARNING, "FFI Parser: %s", message);
	}

	efree(message);
	va_end(va);
}
/* }}} */

static int zend_ffi_validate_vla(zend_ffi_type *type) /* {{{ */
{
	if (!FFI_G(allow_vla) && (type->attr & ZEND_FFI_ATTR_VLA)) {
		zend_ffi_throw_parser_error("'[*]' not allowed in other than function prototype scope at line %d", FFI_G(line));
		return FAILURE;
	}
	return SUCCESS;
}
/* }}} */

static int zend_ffi_validate_incomplete_type(zend_ffi_type *type, zend_bool allow_ic) /* {{{ */
{
	if (type->attr & ZEND_FFI_ATTR_INCOMPLETE_TAG) {
		if (FFI_G(tags)) {
			zend_string *key;
			zend_ffi_tag *tag;

			ZEND_HASH_FOREACH_STR_KEY_PTR(FFI_G(tags), key, tag) {
				if (ZEND_FFI_TYPE(tag->type) == type) {
					if (type->kind == ZEND_FFI_TYPE_ENUM) {
						zend_ffi_throw_parser_error("incomplete 'enum %s' at line %d", ZSTR_VAL(key), FFI_G(line));
					} else if (type->attr & ZEND_FFI_ATTR_UNION) {
						zend_ffi_throw_parser_error("incomplete 'union %s' at line %d", ZSTR_VAL(key), FFI_G(line));
					} else {
						zend_ffi_throw_parser_error("incomplete 'struct %s' at line %d", ZSTR_VAL(key), FFI_G(line));
					}
					return FAILURE;
				}
			} ZEND_HASH_FOREACH_END();
		}
		if (FFI_G(symbols)) {
			zend_string *key;
			zend_ffi_symbol *sym;

			ZEND_HASH_FOREACH_STR_KEY_PTR(FFI_G(symbols), key, sym) {
				if (type == ZEND_FFI_TYPE(sym->type)) {
					zend_ffi_throw_parser_error("incomplete C type '%s' at line %d", ZSTR_VAL(key), FFI_G(line));
					return FAILURE;
				}
			} ZEND_HASH_FOREACH_END();
		}
		zend_ffi_throw_parser_error("incomplete type at line %d", FFI_G(line));
		return FAILURE;
	} else if (!allow_ic && type->attr & ZEND_FFI_ATTR_INCOMPLETE_ARRAY) {
		zend_ffi_throw_parser_error("'[]' not allowed at line %d", FFI_G(line));
		return FAILURE;
	} else if (!FFI_G(allow_vla) && (type->attr & ZEND_FFI_ATTR_VLA)) {
		zend_ffi_throw_parser_error("'[*]' not allowed in other than function prototype scope at line %d", FFI_G(line));
		return FAILURE;
	}
	return SUCCESS;
}
/* }}} */

static int zend_ffi_validate_type(zend_ffi_type *type, zend_bool allow_ic) /* {{{ */
{
	if (type->kind == ZEND_FFI_TYPE_VOID) {
		zend_ffi_throw_parser_error("'void' type is not allowed at line %d", FFI_G(line));
		return FAILURE;
	}
	return zend_ffi_validate_incomplete_type(type, allow_ic);
}
/* }}} */

static int zend_ffi_validate_var_type(zend_ffi_type *type, zend_bool allow_ic) /* {{{ */
{
	if (type->kind == ZEND_FFI_TYPE_FUNC) {
		zend_ffi_throw_parser_error("'function' type is not allowed at line %d", FFI_G(line));
		return FAILURE;
	}
	return zend_ffi_validate_type(type, allow_ic);
}
/* }}} */

void zend_ffi_validate_type_name(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_finalize_type(dcl);
	if (zend_ffi_validate_var_type(ZEND_FFI_TYPE(dcl->type), 0) != SUCCESS) {
		zend_ffi_cleanup_dcl(dcl);
		LONGJMP(FFI_G(bailout), FAILURE);
	}
}
/* }}} */

static int zend_ffi_subst_type(zend_ffi_type **dcl, zend_ffi_type *type) /* {{{ */
{
	zend_ffi_type *dcl_type;
	zend_ffi_field *field;

	if (*dcl == type) {
		*dcl = ZEND_FFI_TYPE_MAKE_OWNED(type);
		return 1;
	}
	dcl_type = *dcl;
	switch (dcl_type->kind) {
		case ZEND_FFI_TYPE_POINTER:
			return zend_ffi_subst_type(&dcl_type->pointer.type, type);
		case ZEND_FFI_TYPE_ARRAY:
			return zend_ffi_subst_type(&dcl_type->array.type, type);
		case ZEND_FFI_TYPE_FUNC:
			if (zend_ffi_subst_type(&dcl_type->func.ret_type, type)) {
				return 1;
			}
			if (dcl_type->func.args) {
				zval *zv;

				ZEND_HASH_FOREACH_VAL(dcl_type->func.args, zv) {
					if (zend_ffi_subst_type((zend_ffi_type**)&Z_PTR_P(zv), type)) {
						return 1;
					}
				} ZEND_HASH_FOREACH_END();
			}
			break;
		case ZEND_FFI_TYPE_STRUCT:
			ZEND_HASH_FOREACH_PTR(&dcl_type->record.fields, field) {
				if (zend_ffi_subst_type(&field->type, type)) {
					return 1;
				}
			} ZEND_HASH_FOREACH_END();
			break;
		default:
			break;
	}
	return 0;
} /* }}} */

static void zend_ffi_tags_cleanup(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_tag *tag;
	ZEND_HASH_FOREACH_PTR(FFI_G(tags), tag) {
		if (ZEND_FFI_TYPE_IS_OWNED(tag->type)) {
			zend_ffi_type *type = ZEND_FFI_TYPE(tag->type);
			zend_ffi_subst_type(&dcl->type, type);
			tag->type = type;
		}
	} ZEND_HASH_FOREACH_END();
	zend_hash_destroy(FFI_G(tags));
	efree(FFI_G(tags));
}
/* }}} */

ZEND_METHOD(FFI, new) /* {{{ */
{
	zend_string *type_def = NULL;
	zval *ztype;
	zend_ffi_type *type, *type_ptr;
	zend_ffi_cdata *cdata;
	void *ptr;
	zend_bool persistent = 0;
	zend_bool is_const = 0;
	zend_ffi_flags flags = ZEND_FFI_FLAG_OWNED;

	ZEND_PARSE_PARAMETERS_START(1, 2)
		if (Z_TYPE_P(EX_VAR_NUM(0)) == IS_STRING) {
			Z_PARAM_STR(type_def)
		} else {
			Z_PARAM_OBJECT_OF_CLASS(ztype, zend_ffi_ctype_ce)
		}
		Z_PARAM_OPTIONAL
		Z_PARAM_BOOL(persistent)
	ZEND_PARSE_PARAMETERS_END();

	if (persistent) {
		flags |= ZEND_FFI_FLAG_PERSISTENT;
	}

	if (type_def) {
		zend_ffi_dcl dcl = {0, 0, 0, 0, NULL};

		if (Z_TYPE(EX(This)) == IS_OBJECT) {
			zend_ffi *ffi = (zend_ffi*)Z_OBJ(EX(This));
			FFI_G(symbols) = ffi->symbols;
			FFI_G(tags) = ffi->tags;
		} else {
			FFI_G(symbols) = NULL;
			FFI_G(tags) = NULL;
		}

		if (zend_ffi_parse_type(ZSTR_VAL(type_def), ZSTR_LEN(type_def), &dcl) != SUCCESS) {
			zend_ffi_type_dtor(dcl.type);
			if (Z_TYPE(EX(This)) != IS_OBJECT) {
				if (FFI_G(tags)) {
					zend_hash_destroy(FFI_G(tags));
					efree(FFI_G(tags));
					FFI_G(tags) = NULL;
				}
				if (FFI_G(symbols)) {
					zend_hash_destroy(FFI_G(symbols));
					efree(FFI_G(symbols));
					FFI_G(symbols) = NULL;
				}
			}
			return;
		}

		type = ZEND_FFI_TYPE(dcl.type);
		if (dcl.attr & ZEND_FFI_ATTR_CONST) {
			is_const = 1;
		}

		if (Z_TYPE(EX(This)) != IS_OBJECT) {
			if (FFI_G(tags)) {
				zend_ffi_tags_cleanup(&dcl);
			}
			if (FFI_G(symbols)) {
				zend_hash_destroy(FFI_G(symbols));
				efree(FFI_G(symbols));
				FFI_G(symbols) = NULL;
			}
		}
		FFI_G(symbols) = NULL;
		FFI_G(tags) = NULL;

		type_ptr = dcl.type;
	} else {
		zend_ffi_ctype *ctype = (zend_ffi_ctype*)Z_OBJ_P(ztype);

		if (ZEND_FFI_TYPE_IS_OWNED(ctype->type)
		 && GC_REFCOUNT(&ctype->std) == 1) {
			/* transfer type ownership */
			type_ptr = ctype->type;
			type = ZEND_FFI_TYPE(type_ptr);
			ctype->type = type;
		} else {
			type_ptr = type = ZEND_FFI_TYPE(ctype->type);
		}
	}

	ptr = pemalloc(type->size, flags & ZEND_FFI_FLAG_PERSISTENT);
	memset(ptr, 0, type->size);

	cdata = (zend_ffi_cdata*)zend_ffi_cdata_new(zend_ffi_cdata_ce);
	if (type->kind < ZEND_FFI_TYPE_POINTER) {
		cdata->std.handlers = &zend_ffi_cdata_value_handlers;
	}
	cdata->type = type_ptr;
	cdata->ptr = ptr;
	cdata->flags = flags;
	if (is_const) {
		cdata->flags |= ZEND_FFI_FLAG_CONST;
	}

	RETURN_OBJ(&cdata->std);
}
/* }}} */

ZEND_METHOD(FFI, own) /* {{{ */
{
	zval *zv;
	zend_ffi_cdata *cdata;
	zend_bool own = 1;

	ZEND_PARSE_PARAMETERS_START(1, 2)
		Z_PARAM_OBJECT_OF_CLASS_EX2(zv, zend_ffi_cdata_ce, 0, 1, 0);
		Z_PARAM_OPTIONAL
		Z_PARAM_BOOL(own);
	ZEND_PARSE_PARAMETERS_END();

	cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);
	if (own) {
		cdata->flags |= ZEND_FFI_FLAG_OWNED;
	} else {
		cdata->flags &= ~ZEND_FFI_FLAG_OWNED;
	}
	ZVAL_COPY(return_value, zv);
}
/* }}} */

ZEND_METHOD(FFI, free) /* {{{ */
{
	zval *zv;
	zend_ffi_cdata *cdata;

	ZEND_PARSE_PARAMETERS_START(1, 1)
		Z_PARAM_OBJECT_OF_CLASS_EX2(zv, zend_ffi_cdata_ce, 0, 1, 0);
	ZEND_PARSE_PARAMETERS_END();

	cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);

	if (ZEND_FFI_TYPE(cdata->type)->kind == ZEND_FFI_TYPE_POINTER) {
		if (!cdata->ptr) {
			zend_throw_error(zend_ffi_exception_ce, "NULL pointer dereference");
			return;
		}
		if (cdata->ptr != (void*)&cdata->ptr_holder) {
			pefree(*(void**)cdata->ptr, cdata->flags & ZEND_FFI_FLAG_PERSISTENT);
		}
		*(void**)cdata->ptr = NULL;
	} else if (!(cdata->flags & ZEND_FFI_FLAG_OWNED)) {
		pefree(cdata->ptr, cdata->flags & ZEND_FFI_FLAG_PERSISTENT);
		cdata->ptr = NULL;
		cdata->flags &= ~(ZEND_FFI_FLAG_OWNED|ZEND_FFI_FLAG_PERSISTENT);
	} else {
		zend_throw_error(zend_ffi_exception_ce, "free() non a C pointer");
	}
}
/* }}} */

ZEND_METHOD(FFI, cast) /* {{{ */
{
	zend_string *type_def = NULL;
	zval *ztype;
	zend_ffi_type *old_type, *type, *type_ptr;
	zend_ffi_cdata *old_cdata, *cdata;
	zend_bool is_const = 0;
	zval *zv;
	void *ptr;

	ZEND_PARSE_PARAMETERS_START(2, 2)
		if (Z_TYPE_P(EX_VAR_NUM(0)) == IS_STRING) {
			Z_PARAM_STR(type_def)
		} else {
			Z_PARAM_OBJECT_OF_CLASS(ztype, zend_ffi_ctype_ce)
		}
		Z_PARAM_OBJECT_OF_CLASS(zv, zend_ffi_cdata_ce)
	ZEND_PARSE_PARAMETERS_END();

	if (type_def) {
		zend_ffi_dcl dcl = {0, 0, 0, 0, NULL};

		if (Z_TYPE(EX(This)) == IS_OBJECT) {
			zend_ffi *ffi = (zend_ffi*)Z_OBJ(EX(This));
			FFI_G(symbols) = ffi->symbols;
			FFI_G(tags) = ffi->tags;
		} else {
			FFI_G(symbols) = NULL;
			FFI_G(tags) = NULL;
		}

		if (zend_ffi_parse_type(ZSTR_VAL(type_def), ZSTR_LEN(type_def), &dcl) != SUCCESS) {
			zend_ffi_type_dtor(dcl.type);
			if (Z_TYPE(EX(This)) != IS_OBJECT) {
				if (FFI_G(tags)) {
					zend_hash_destroy(FFI_G(tags));
					efree(FFI_G(tags));
					FFI_G(tags) = NULL;
				}
				if (FFI_G(symbols)) {
					zend_hash_destroy(FFI_G(symbols));
					efree(FFI_G(symbols));
					FFI_G(symbols) = NULL;
				}
			}
			return;
		}

		type = ZEND_FFI_TYPE(dcl.type);
		if (dcl.attr & ZEND_FFI_ATTR_CONST) {
			is_const = 1;
		}

		if (Z_TYPE(EX(This)) != IS_OBJECT) {
			if (FFI_G(tags)) {
				zend_ffi_tags_cleanup(&dcl);
			}
			if (FFI_G(symbols)) {
				zend_hash_destroy(FFI_G(symbols));
				efree(FFI_G(symbols));
				FFI_G(symbols) = NULL;
			}
		}
		FFI_G(symbols) = NULL;
		FFI_G(tags) = NULL;

		type_ptr = dcl.type;
	} else {
		zend_ffi_ctype *ctype = (zend_ffi_ctype*)Z_OBJ_P(ztype);

		if (ZEND_FFI_TYPE_IS_OWNED(ctype->type)
		 && GC_REFCOUNT(&ctype->std) == 1) {
			/* transfer type ownership */
			type_ptr = ctype->type;
			type = ZEND_FFI_TYPE(type_ptr);
			ctype->type = type;
		} else {
			type_ptr = type = ZEND_FFI_TYPE(ctype->type);
		}
	}

	old_cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);
	old_type = ZEND_FFI_TYPE(old_cdata->type);
	ptr = old_cdata->ptr;

	if (old_type->kind == ZEND_FFI_TYPE_POINTER
	 && type->kind != ZEND_FFI_TYPE_POINTER
	 && ZEND_FFI_TYPE(old_type->pointer.type)->kind == ZEND_FFI_TYPE_VOID) {
		/* automatically dereference void* pointers ??? */
		ptr = *(void**)ptr;
	} else if (type->size > old_type->size) {
		zend_throw_error(zend_ffi_exception_ce, "attempt to cast to larger type");
		return;
	}

	cdata = (zend_ffi_cdata*)zend_ffi_cdata_new(zend_ffi_cdata_ce);
	if (type->kind < ZEND_FFI_TYPE_POINTER) {
		cdata->std.handlers = &zend_ffi_cdata_value_handlers;
	}
	cdata->type = type_ptr;
	cdata->ptr = ptr;
	if (is_const) {
		cdata->flags |= ZEND_FFI_FLAG_CONST;
	}

	if (old_cdata->flags & ZEND_FFI_FLAG_OWNED) {
		if (GC_REFCOUNT(&old_cdata->std) == 1) {
			/* transfer ownership */
			old_cdata->flags &= ~ZEND_FFI_FLAG_OWNED;
			cdata->flags |= ZEND_FFI_FLAG_OWNED;
		} else {
			//???zend_throw_error(zend_ffi_exception_ce, "Attempt to cast owned C pointer");
		}
	}

	RETURN_OBJ(&cdata->std);
}
/* }}} */

ZEND_METHOD(FFI, type) /* {{{ */
{
	zval *zv;
	zend_ffi_ctype *ctype;
	zend_ffi_type *type;

	ZEND_PARSE_PARAMETERS_START(1, 1)
		Z_PARAM_ZVAL(zv);
	ZEND_PARSE_PARAMETERS_END();

	if (Z_TYPE_P(zv) == IS_STRING) {
		zend_string *type_def = Z_STR_P(zv);

		zend_ffi_dcl dcl = {0, 0, 0, 0, NULL};

		if (Z_TYPE(EX(This)) == IS_OBJECT) {
			zend_ffi *ffi = (zend_ffi*)Z_OBJ(EX(This));
			FFI_G(symbols) = ffi->symbols;
			FFI_G(tags) = ffi->tags;
		} else {
			FFI_G(symbols) = NULL;
			FFI_G(tags) = NULL;
		}

		if (zend_ffi_parse_type(ZSTR_VAL(type_def), ZSTR_LEN(type_def), &dcl) != SUCCESS) {
			zend_ffi_type_dtor(dcl.type);
			if (Z_TYPE(EX(This)) != IS_OBJECT) {
				if (FFI_G(tags)) {
					zend_hash_destroy(FFI_G(tags));
					efree(FFI_G(tags));
					FFI_G(tags) = NULL;
				}
				if (FFI_G(symbols)) {
					zend_hash_destroy(FFI_G(symbols));
					efree(FFI_G(symbols));
					FFI_G(symbols) = NULL;
				}
			}
			return;
		}

		if (Z_TYPE(EX(This)) != IS_OBJECT) {
			if (FFI_G(tags)) {
				zend_ffi_tags_cleanup(&dcl);
			}
			if (FFI_G(symbols)) {
				zend_hash_destroy(FFI_G(symbols));
				efree(FFI_G(symbols));
				FFI_G(symbols) = NULL;
			}
		}
		FFI_G(symbols) = NULL;
		FFI_G(tags) = NULL;

		type = dcl.type;
	} else if (Z_TYPE_P(zv) == IS_OBJECT && Z_OBJCE_P(zv) == zend_ffi_cdata_ce) {
		zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);

		type = ZEND_FFI_TYPE(cdata->type);
	} else {
		zend_wrong_parameter_class_error(1, "FFI\\CData or string", zv);
		return;
	}

	ctype = (zend_ffi_ctype*)zend_ffi_ctype_new(zend_ffi_ctype_ce);
	ctype->type = type;

	RETURN_OBJ(&ctype->std);
}
/* }}} */

ZEND_METHOD(FFI, array) /* {{{ */
{
	zval *ztype;
	zend_ffi_ctype *ctype;
	zend_ffi_type *type;
	HashTable *dims;
	zval *val;

	ZEND_PARSE_PARAMETERS_START(2, 2)
		Z_PARAM_OBJECT_OF_CLASS(ztype, zend_ffi_ctype_ce)
		Z_PARAM_ARRAY_HT(dims)
	ZEND_PARSE_PARAMETERS_END();

	ctype = (zend_ffi_ctype*)Z_OBJ_P(ztype);
	type = ZEND_FFI_TYPE(ctype->type);

	if (type->kind == ZEND_FFI_TYPE_FUNC) {
		zend_throw_error(zend_ffi_exception_ce, "array of functions is not allowed");
		return;
	} else if (type->kind == ZEND_FFI_TYPE_ARRAY && (type->attr & ZEND_FFI_ATTR_INCOMPLETE_ARRAY)) {
		zend_throw_error(zend_ffi_exception_ce, "only the leftmost array can be undimensioned");
		return;
	} else if (type->kind == ZEND_FFI_TYPE_VOID) {
		zend_throw_error(zend_ffi_exception_ce, "array of 'void' is not allowed");
		return;
	} else if (type->attr & ZEND_FFI_ATTR_INCOMPLETE_TAG) {
		zend_throw_error(zend_ffi_exception_ce, "array of incomplete type is not allowed");
		return;
	}

	ZEND_HASH_REVERSE_FOREACH_VAL(dims, val) {
		zend_long n = zval_get_long(val);
		zend_ffi_type *new_type;

		if (n < 0) {
			zend_throw_error(zend_ffi_exception_ce, "negative array index");
			zend_ffi_type_dtor(type);
			return;
		} else if (ZEND_FFI_TYPE(type)->kind == ZEND_FFI_TYPE_ARRAY && (ZEND_FFI_TYPE(type)->attr & ZEND_FFI_ATTR_INCOMPLETE_ARRAY)) {
			zend_throw_error(zend_ffi_exception_ce, "only the leftmost array can be undimensioned");
			zend_ffi_type_dtor(type);
			return;
		}

		new_type = emalloc(sizeof(zend_ffi_type));
		new_type->kind = ZEND_FFI_TYPE_ARRAY;
		new_type->attr = 0;
		new_type->size = n * ZEND_FFI_TYPE(type)->size;
		new_type->align = ZEND_FFI_TYPE(type)->align;
		new_type->array.type = type;
		new_type->array.length = n;

		if (n == 0) {
			new_type->attr |= ZEND_FFI_ATTR_INCOMPLETE_ARRAY;
		}

		type = ZEND_FFI_TYPE_MAKE_OWNED(new_type);
	} ZEND_HASH_FOREACH_END();

	ctype = (zend_ffi_ctype*)zend_ffi_ctype_new(zend_ffi_ctype_ce);
	ctype->type = type;

	RETURN_OBJ(&ctype->std);
}
/* }}} */

ZEND_METHOD(FFI, addr) /* {{{ */
{
	zend_ffi_type *type, *new_type;
	zend_ffi_cdata *cdata, *new_cdata;
	zval *zv;

	ZEND_PARSE_PARAMETERS_START(1, 1)
		Z_PARAM_OBJECT_OF_CLASS(zv, zend_ffi_cdata_ce)
	ZEND_PARSE_PARAMETERS_END();

	cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);
	type = ZEND_FFI_TYPE(cdata->type);

	new_type = emalloc(sizeof(zend_ffi_type));
	new_type->kind = ZEND_FFI_TYPE_POINTER;
	new_type->attr = 0;
	new_type->size = sizeof(void*);
	new_type->align = _Alignof(void*);
	new_type->pointer.type = type; /* life-time ??? */

	new_cdata = (zend_ffi_cdata*)zend_ffi_cdata_new(zend_ffi_cdata_ce);
	new_cdata->type = ZEND_FFI_TYPE_MAKE_OWNED(new_type);
	new_cdata->ptr = &cdata->ptr;

	RETURN_OBJ(&new_cdata->std);
}
/* }}} */

ZEND_METHOD(FFI, offset) /* {{{ */
{
	zend_ffi_type *type, *new_type;
	zend_ffi_cdata *cdata, *new_cdata;
	zval *zv;
	zend_long offset;

	ZEND_PARSE_PARAMETERS_START(2, 2)
		Z_PARAM_OBJECT_OF_CLASS(zv, zend_ffi_cdata_ce)
		Z_PARAM_LONG(offset)
	ZEND_PARSE_PARAMETERS_END();

	cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);
	type = ZEND_FFI_TYPE(cdata->type);

	if (type->kind == ZEND_FFI_TYPE_POINTER) {
		new_cdata = (zend_ffi_cdata*)zend_ffi_cdata_new(zend_ffi_cdata_ce);
		new_cdata->type = type;
		new_cdata->ptr = (void*)&new_cdata->ptr_holder;

		*(void**)new_cdata->ptr = ((char*)(*(void**)cdata->ptr)) +
			(ZEND_FFI_TYPE(new_type->pointer.type)->size * offset);
	} else if (type->kind == ZEND_FFI_TYPE_ARRAY) {
		new_type = emalloc(sizeof(zend_ffi_type));
		new_type->kind = ZEND_FFI_TYPE_POINTER;
		new_type->attr = 0;
		new_type->size = sizeof(void*);
		new_type->align = _Alignof(void*);
		new_type->pointer.type = type->array.type; /* life-time ??? */

		new_cdata = (zend_ffi_cdata*)zend_ffi_cdata_new(zend_ffi_cdata_ce);
		new_cdata->type = ZEND_FFI_TYPE_MAKE_OWNED(new_type);
		new_cdata->ptr = (void*)&new_cdata->ptr_holder;

		*(void**)new_cdata->ptr = ((char*)cdata->ptr) +
			(ZEND_FFI_TYPE(new_type->pointer.type)->size * offset);
	} else {
		zend_throw_error(zend_ffi_exception_ce, "offset() works only with pinters and arrays");
	}

	RETURN_OBJ(&new_cdata->std);
}
/* }}} */

ZEND_METHOD(FFI, sizeof) /* {{{ */
{
	zval *zv;
	zend_ffi_type *type;

	ZEND_PARSE_PARAMETERS_START(1, 1)
		Z_PARAM_ZVAL(zv);
	ZEND_PARSE_PARAMETERS_END();

	if (Z_TYPE_P(zv) == IS_OBJECT && Z_OBJCE_P(zv) == zend_ffi_cdata_ce) {
		zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);
		type = ZEND_FFI_TYPE(cdata->type);
	} else if (Z_TYPE_P(zv) == IS_OBJECT && Z_OBJCE_P(zv) == zend_ffi_ctype_ce) {
		zend_ffi_ctype *ctype = (zend_ffi_ctype*)Z_OBJ_P(zv);
		type = ZEND_FFI_TYPE(ctype->type);
	} else {
		zend_wrong_parameter_class_error(1, "FFI\\CData or FFI\\CType", zv);
		return;
	}

	RETURN_LONG(type->size);
}
/* }}} */

ZEND_METHOD(FFI, alignof) /* {{{ */
{
	zval *zv;
	zend_ffi_type *type;

	ZEND_PARSE_PARAMETERS_START(1, 1)
		Z_PARAM_ZVAL(zv);
	ZEND_PARSE_PARAMETERS_END();

	if (Z_TYPE_P(zv) == IS_OBJECT && Z_OBJCE_P(zv) == zend_ffi_cdata_ce) {
		zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);
		type = ZEND_FFI_TYPE(cdata->type);
	} else if (Z_TYPE_P(zv) == IS_OBJECT && Z_OBJCE_P(zv) == zend_ffi_ctype_ce) {
		zend_ffi_ctype *ctype = (zend_ffi_ctype*)Z_OBJ_P(zv);
		type = ZEND_FFI_TYPE(ctype->type);
	} else {
		zend_wrong_parameter_class_error(1, "FFI\\CData or FFI\\CType", zv);
		return;
	}

	RETURN_LONG(type->align);
}
/* }}} */

ZEND_METHOD(FFI, memcpy) /* {{{ */
{
	zval *zv1, *zv2 = NULL;
	zend_ffi_cdata *cdata1, *cdata2;
	zend_ffi_type *type1, *type2;
	void *ptr1, *ptr2;
	zend_long size;
	zend_string *str2 = NULL;

	ZEND_PARSE_PARAMETERS_START(3, 3)
		Z_PARAM_OBJECT_OF_CLASS(zv1, zend_ffi_cdata_ce)
		if (Z_TYPE_P(EX_VAR_NUM(1)) == IS_STRING) {
			Z_PARAM_STR(str2)
		} else {
			Z_PARAM_OBJECT_OF_CLASS(zv2, zend_ffi_cdata_ce)
		}
		Z_PARAM_LONG(size)
	ZEND_PARSE_PARAMETERS_END();

	cdata1 = (zend_ffi_cdata*)Z_OBJ_P(zv1);
	type1 = ZEND_FFI_TYPE(cdata1->type);
	if (type1->kind == ZEND_FFI_TYPE_POINTER) {
		ptr1 = *(void**)cdata1->ptr;
	} else {
		ptr1 = cdata1->ptr;
		if (type1->kind != ZEND_FFI_TYPE_POINTER && size > type1->size) {
			zend_throw_error(zend_ffi_exception_ce, "attempt to write over data boundary");
			return;
		}
	}
	if (str2) {
		ptr2 = ZSTR_VAL(str2);
		if (size > ZSTR_LEN(str2)) {
			zend_throw_error(zend_ffi_exception_ce, "attempt to read over string boundary");
			return;
		}
	} else {
		cdata2 = (zend_ffi_cdata*)Z_OBJ_P(zv2);
		type2 = ZEND_FFI_TYPE(cdata2->type);
		if (type2->kind == ZEND_FFI_TYPE_POINTER) {
			ptr2 = *(void**)cdata2->ptr;
		} else {
			ptr2 = cdata2->ptr;
			if (type2->kind != ZEND_FFI_TYPE_POINTER && size > type2->size) {
				zend_throw_error(zend_ffi_exception_ce, "attempt to read over data boundary");
				return;
			}
		}
	}

	memcpy(ptr1, ptr2, size);
}
/* }}} */

ZEND_METHOD(FFI, memcmp) /* {{{ */
{
	zval *zv1 = NULL, *zv2 = NULL;
	zend_ffi_cdata *cdata1, *cdata2;
	zend_ffi_type *type1, *type2;
	void *ptr1, *ptr2;
	zend_long size;
	int ret;
	zend_string *str1 = NULL;
	zend_string *str2 = NULL;

	ZEND_PARSE_PARAMETERS_START(3, 3)
		if (Z_TYPE_P(EX_VAR_NUM(0)) == IS_STRING) {
			Z_PARAM_STR(str1)
		} else {
			Z_PARAM_OBJECT_OF_CLASS(zv1, zend_ffi_cdata_ce)
		}
		if (Z_TYPE_P(EX_VAR_NUM(1)) == IS_STRING) {
			Z_PARAM_STR(str2)
		} else {
			Z_PARAM_OBJECT_OF_CLASS(zv2, zend_ffi_cdata_ce)
		}
		Z_PARAM_LONG(size)
	ZEND_PARSE_PARAMETERS_END();

	if (str1) {
		ptr1 = ZSTR_VAL(str1);
		if (size > ZSTR_LEN(str1)) {
			zend_throw_error(zend_ffi_exception_ce, "attempt to read over string boundary");
			return;
		}
	} else {
		cdata1 = (zend_ffi_cdata*)Z_OBJ_P(zv1);
		type1 = ZEND_FFI_TYPE(cdata1->type);
		if (type1->kind == ZEND_FFI_TYPE_POINTER) {
			ptr1 = *(void**)cdata1->ptr;
		} else {
			ptr1 = cdata1->ptr;
			if (type1->kind != ZEND_FFI_TYPE_POINTER && size > type1->size) {
				zend_throw_error(zend_ffi_exception_ce, "attempt to read over data boundary");
				return;
			}
		}
	}
	if (str2) {
		ptr2 = ZSTR_VAL(str2);
		if (size > ZSTR_LEN(str2)) {
			zend_throw_error(zend_ffi_exception_ce, "attempt to read over string boundary");
			return;
		}
	} else {
		cdata2 = (zend_ffi_cdata*)Z_OBJ_P(zv2);
		type2 = ZEND_FFI_TYPE(cdata2->type);
		if (type2->kind == ZEND_FFI_TYPE_POINTER) {
			ptr2 = *(void**)cdata2->ptr;
		} else {
			ptr2 = cdata2->ptr;
			if (type2->kind != ZEND_FFI_TYPE_POINTER && size > type2->size) {
				zend_throw_error(zend_ffi_exception_ce, "attempt to read over data boundary");
				return;
			}
		}
	}

	ret = memcmp(ptr1, ptr2, size);
	if (ret == 0) {
		RETVAL_LONG(0);
	} else if (ret < 0) {
		RETVAL_LONG(-1);
	} else {
		RETVAL_LONG(1);
	}
}
/* }}} */

ZEND_METHOD(FFI, memset) /* {{{ */
{
	zval *zv;
	zend_ffi_cdata *cdata;
	zend_ffi_type *type;
	void *ptr;
	zend_long ch, size;

	ZEND_PARSE_PARAMETERS_START(3, 3)
		Z_PARAM_OBJECT_OF_CLASS(zv, zend_ffi_cdata_ce)
		Z_PARAM_LONG(ch)
		Z_PARAM_LONG(size)
	ZEND_PARSE_PARAMETERS_END();

	cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);
	type = ZEND_FFI_TYPE(cdata->type);
	if (type->kind == ZEND_FFI_TYPE_POINTER) {
		ptr = *(void**)cdata->ptr;
	} else {
		ptr = cdata->ptr;
		if (type->kind != ZEND_FFI_TYPE_POINTER && size > type->size) {
			zend_throw_error(zend_ffi_exception_ce, "attempt to write over data boundary");
			return;
		}
	}

	memset(ptr, ch, size);
}
/* }}} */

ZEND_METHOD(FFI, string) /* {{{ */
{
	zval *zv;
	zend_ffi_cdata *cdata;
	zend_ffi_type *type;
	void *ptr;
	zend_long size = 0;

	ZEND_PARSE_PARAMETERS_START(1, 2)
		Z_PARAM_OBJECT_OF_CLASS(zv, zend_ffi_cdata_ce)
		Z_PARAM_OPTIONAL
		Z_PARAM_LONG(size)
	ZEND_PARSE_PARAMETERS_END();

	cdata = (zend_ffi_cdata*)Z_OBJ_P(zv);
	type = ZEND_FFI_TYPE(cdata->type);
	if (EX_NUM_ARGS() == 2) {
		if (type->kind == ZEND_FFI_TYPE_POINTER) {
			ptr = *(void**)cdata->ptr;
		} else {
			ptr = cdata->ptr;
			if (type->kind != ZEND_FFI_TYPE_POINTER && size > type->size) {
				zend_throw_error(zend_ffi_exception_ce, "attempt to read over data boundary");
				return;
			}
		}
		RETURN_STRINGL((char*)ptr, size);
	} else {
		if (type->kind == ZEND_FFI_TYPE_POINTER && ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_CHAR) {
			ptr = *(void**)cdata->ptr;
		} else if (type->kind == ZEND_FFI_TYPE_ARRAY && ZEND_FFI_TYPE(type->array.type)->kind == ZEND_FFI_TYPE_CHAR) {
			ptr = cdata->ptr;
		} else {
			zend_throw_error(zend_ffi_exception_ce, "FFI\\Cdata is not a C string");
			return;
		}
		RETURN_STRING((char*)ptr);
	}
}
/* }}} */

ZEND_BEGIN_ARG_INFO_EX(arginfo_func_own, 0, 0, 1)
	ZEND_ARG_INFO(ZEND_SEND_PREFER_REF, ptr)
	ZEND_ARG_INFO(0, own)
ZEND_END_ARG_INFO()

ZEND_BEGIN_ARG_INFO_EX(arginfo_func_free, 0, 0, 1)
	ZEND_ARG_INFO(ZEND_SEND_PREFER_REF, ptr)
ZEND_END_ARG_INFO()

static const zend_function_entry zend_ffi_functions[] = {
	ZEND_ME(FFI, __construct, NULL,              ZEND_ACC_PUBLIC)
	ZEND_ME(FFI, load,        NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, scope,       NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, new,         NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, own,         arginfo_func_own,  ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, free,        arginfo_func_free, ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, cast,        NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, type,        NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, array,       NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, addr,        NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, offset,      NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, sizeof,      NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, alignof,     NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, memcpy,      NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, memcmp,      NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, memset,      NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_ME(FFI, string,      NULL,              ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_FE_END
};

ZEND_INI_BEGIN()
	STD_ZEND_INI_ENTRY("ffi.preload", NULL, PHP_INI_SYSTEM, OnUpdateString,
                  preload, zend_ffi_globals, ffi_globals)
ZEND_INI_END()

static int (*orig_post_startup_cb)(void);

static int zend_ffi_same_types(zend_ffi_type *old, zend_ffi_type *type) /* {{{ */
{
	if (old == type) {
		return 1;
	}

	if (old->kind != type->kind
	 || old->size != type->size
	 || old->align != type->align
	 || old->attr != type->attr) {
		return 0;
	}

	switch (old->kind) {
		case ZEND_FFI_TYPE_ENUM:
			return old->enumeration.kind == type->enumeration.kind;
		case ZEND_FFI_TYPE_ARRAY:
			return old->array.length == type->array.length
			 &&	zend_ffi_same_types(ZEND_FFI_TYPE(old->array.type), ZEND_FFI_TYPE(type->array.type));
		case ZEND_FFI_TYPE_POINTER:
			return zend_ffi_same_types(ZEND_FFI_TYPE(old->pointer.type), ZEND_FFI_TYPE(type->pointer.type));
		case ZEND_FFI_TYPE_STRUCT:
			if (zend_hash_num_elements(&old->record.fields) != zend_hash_num_elements(&type->record.fields)) {
				return 0;
			} else {
				zend_ffi_field *old_field, *field;
				zend_string *key;
				Bucket *b = type->record.fields.arData;

				ZEND_HASH_FOREACH_STR_KEY_PTR(&old->record.fields, key, old_field) {
					while (Z_TYPE(b->val) == IS_UNDEF) {
						b++;
					}
					if (key) {
						if (!b->key
						 || !zend_string_equals(key, b->key)) {
							return 0;
						}
					} else if (b->key) {
						return 0;
					}
					field = Z_PTR(b->val);
					if (old_field->offset != field->offset
					 || old_field->is_const != field->is_const
					 || old_field->is_nested != field->is_nested
					 || old_field->first_bit != field->first_bit
					 || old_field->bits != field->bits
					 || !zend_ffi_same_types(ZEND_FFI_TYPE(old_field->type), ZEND_FFI_TYPE(field->type))) {
						return 0;
					}
					b++;
				} ZEND_HASH_FOREACH_END();
			}
			break;
		case ZEND_FFI_TYPE_FUNC:
			if (old->func.abi != type->func.abi
			 || ((old->func.args ? zend_hash_num_elements(old->func.args) : 0) != (type->func.args ? zend_hash_num_elements(type->func.args) : 0))
			 || !zend_ffi_same_types(ZEND_FFI_TYPE(old->func.ret_type), ZEND_FFI_TYPE(type->func.ret_type))) {
				return 0;
			} else if (old->func.args) {
				zend_ffi_type *arg_type;
				Bucket *b = type->func.args->arData;

				ZEND_HASH_FOREACH_PTR(old->func.args, arg_type) {
					while (Z_TYPE(b->val) == IS_UNDEF) {
						b++;
					}
					if (!zend_ffi_same_types(ZEND_FFI_TYPE(arg_type), ZEND_FFI_TYPE(Z_PTR(b->val)))) {
						return 0;
					}
					b++;
				} ZEND_HASH_FOREACH_END();
			}
			break;
		default:
			break;
	}

	return 1;
}
/* }}} */

static int zend_ffi_same_symbols(zend_ffi_symbol *old, zend_ffi_symbol *sym) /* {{{ */
{
	if (old->kind != sym->kind || old->is_const != sym->is_const) {
		return 0;
	}

	if (old->kind == ZEND_FFI_SYM_CONST) {
		if (old->value != sym->value) {
			return 0;
		}
	}

	return zend_ffi_same_types(ZEND_FFI_TYPE(old->type), ZEND_FFI_TYPE(sym->type));
}
/* }}} */

static int zend_ffi_same_tags(zend_ffi_tag *old, zend_ffi_tag *tag) /* {{{ */
{
	if (old->kind != tag->kind) {
		return 0;
	}

	return zend_ffi_same_types(ZEND_FFI_TYPE(old->type), ZEND_FFI_TYPE(tag->type));
}
/* }}} */

static int zend_ffi_subst_old_type(zend_ffi_type **dcl, zend_ffi_type *old, zend_ffi_type *type) /* {{{ */
{
	zend_ffi_type *dcl_type;
	zend_ffi_field *field;

	if (ZEND_FFI_TYPE(*dcl) == type) {
		*dcl = old;
		return 1;
	}
	dcl_type = *dcl;
	switch (dcl_type->kind) {
		case ZEND_FFI_TYPE_POINTER:
			return zend_ffi_subst_old_type(&dcl_type->pointer.type, old, type);
		case ZEND_FFI_TYPE_ARRAY:
			return zend_ffi_subst_old_type(&dcl_type->array.type, old, type);
		case ZEND_FFI_TYPE_FUNC:
			if (zend_ffi_subst_old_type(&dcl_type->func.ret_type, old, type)) {
				return 1;
			}
			if (dcl_type->func.args) {
				zval *zv;

				ZEND_HASH_FOREACH_VAL(dcl_type->func.args, zv) {
					if (zend_ffi_subst_old_type((zend_ffi_type**)&Z_PTR_P(zv), old, type)) {
						return 1;
					}
				} ZEND_HASH_FOREACH_END();
			}
			break;
		case ZEND_FFI_TYPE_STRUCT:
			ZEND_HASH_FOREACH_PTR(&dcl_type->record.fields, field) {
				if (zend_ffi_subst_old_type(&field->type, old, type)) {
					return 1;
				}
			} ZEND_HASH_FOREACH_END();
			break;
		default:
			break;
	}
	return 0;
} /* }}} */

static void zend_ffi_cleanup_type(zend_ffi_type *old, zend_ffi_type *type) /* {{{ */
{
	zend_ffi_symbol *sym;
	zend_ffi_tag *tag;

	if (FFI_G(symbols)) {
		ZEND_HASH_FOREACH_PTR(FFI_G(symbols), sym) {
			zend_ffi_subst_old_type(&sym->type, old, type);
		} ZEND_HASH_FOREACH_END();
	}
	if (FFI_G(tags)) {
		ZEND_HASH_FOREACH_PTR(FFI_G(tags), tag) {
			zend_ffi_subst_old_type(&tag->type, old, type);
		} ZEND_HASH_FOREACH_END();
	}
}
/* }}} */

static char *zend_ffi_parse_directives(const char *filename, char *code_pos, char **scope_name, char **lib, zend_bool preload) /* {{{ */
{
	char *p;

	*scope_name = NULL;
	*lib = NULL;
	while (*code_pos == '#') {
		if (strncmp(code_pos, "#define FFI_SCOPE \"", sizeof("#define FFI_SCOPE \"") - 1) == 0) {
			if (*scope_name) {
				if (preload) {
					zend_error(E_WARNING, "FFI: failed pre-loading '%s', FFI_SCOPE defined twice", filename);
				} else {
					zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s', FFI_SCOPE defined twice", filename);
				}
				return NULL;
			}
			*scope_name = p = code_pos + sizeof("#define FFI_SCOPE \"") - 1;
			while (1) {
				if (*p == '\"') {
					*p = 0;
					p++;
					break;
				} else if (*p <= ' ') {
					if (preload) {
						zend_error(E_WARNING, "FFI: failed pre-loading '%s', bad FFI_SCOPE define", filename);
					} else {
						zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s', bad FFI_SCOPE define", filename);
					}
					return NULL;
				}
				p++;
			}
			while (*p == ' ' || *p == '\t') {
				p++;
			}
			while (*p == '\r' || *p == '\n') {
				p++;
			}
			code_pos = p;
		} else if (strncmp(code_pos, "#define FFI_LIB \"", sizeof("#define FFI_LIB \"") - 1) == 0) {
			if (*lib) {
				if (preload) {
					zend_error(E_WARNING, "FFI: failed pre-loading '%s', FFI_LIB defined twice", filename);
				} else {
					zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s', FFI_LIB defined twice", filename);
				}
				return NULL;
			}
			*lib = p = code_pos + sizeof("#define FFI_LIB \"") - 1;
			while (1) {
				if (*p == '\"') {
					*p = 0;
					p++;
					break;
				} else if (*p <= ' ') {
					if (preload) {
						zend_error(E_WARNING, "FFI: failed pre-loading '%s', bad FFI_LIB define", filename);
					} else {
						zend_throw_error(zend_ffi_exception_ce, "Failed loading '%s', bad FFI_LIB define", filename);
					}
					return NULL;
				}
				p++;
			}
			while (*p == ' ' || *p == '\t') {
				p++;
			}
			while (*p == '\r' || *p == '\n') {
				p++;
			}
			code_pos = p;
		} else {
			break;
		}
	}
	return code_pos;
}
/* }}} */

static void zend_ffi_preload(const char *filename) /* {{{ */
{
	struct stat buf;
	int fd;
	char *code, *code_pos, *scope_name, *lib, *p;
	size_t code_size, scope_name_len;
	zend_ffi_scope *scope;

	if (stat(filename, &buf) != 0) {
		zend_error(E_WARNING, "FFI: failed pre-loading '%s', file doesn't exist", filename);
		return;
	}

	if ((buf.st_mode & S_IFMT) == S_IFDIR){
		/* Preload all files with ".h" extension from the directory */
		size_t filename_len = strlen(filename);
		struct dirent **namelist = NULL;
		int nfiles, i;
		char *p;
		char file[MAXPATHLEN];

		nfiles = php_scandir(filename, &namelist, 0, NULL);
		for (i = 0; i < nfiles; i++) {
			p = strrchr(namelist[i]->d_name, '.');
			if (p && p[1] == 'h' && p[2] == 0) {
				if (IS_SLASH(filename[filename_len - 1])) {
					snprintf(file, MAXPATHLEN, "%s%s", filename, namelist[i]->d_name);
				} else {
					snprintf(file, MAXPATHLEN, "%s%c%s", filename, DEFAULT_SLASH, namelist[i]->d_name);
				}
				zend_ffi_preload(file);
			}
			free(namelist[i]);
		}
		if (namelist) {
			free(namelist);
		}

		return;
	} else if ((buf.st_mode & S_IFMT) != S_IFREG) {
		zend_error(E_WARNING, "FFI: failed pre-loading '%s', not a regular file", filename);
		return;
	}

	FFI_G(symbols) = NULL;
	FFI_G(tags) = NULL;

	code_size = buf.st_size;
	code = emalloc(code_size + 1);
	fd = open(filename, O_RDONLY, 0);
	if (fd < 0 || read(fd, code, code_size) != code_size) {
		zend_error(E_WARNING, "FFI: failed pre-loading '%s', cannot read_file", filename);
		goto cleanup;
	}
	close(fd);
	code[code_size] = 0;

	scope_name = NULL;
	lib = NULL;
	code_pos = zend_ffi_parse_directives(filename, code, &scope_name, &lib, 1);
	if (!code_pos) {
		goto cleanup;
	}
	code_size -= code_pos - code;

	if (!scope_name) {
		scope_name = "C";
	}
	scope_name_len = strlen(scope_name);

	if (zend_ffi_parse_decl(code_pos, code_size) != SUCCESS) {
		zend_error(E_WARNING, "FFI: failed pre-loading '%s'", filename);
		goto cleanup;
	}

	scope = NULL;
	if (FFI_G(scopes)) {
		scope = zend_hash_str_find_ptr(FFI_G(scopes), scope_name, scope_name_len);
	}

	if (FFI_G(symbols) || FFI_G(tags)) {
		DL_HANDLE handle = NULL;
		void *addr;
		zend_string *name;
		zend_ffi_symbol *sym;
		zend_ffi_tag *tag;

		if (lib) {
			handle = DL_LOAD(lib);
			if (!handle) {
				zend_error(E_WARNING, "FFI: failed pre-loading '%s', cannot load library '%s'", filename, lib);
				goto cleanup;
			}
#ifdef RTLD_DEFAULT
		} else if (1) {
			// TODO: this might need to be disabled or protected ???
			handle = RTLD_DEFAULT;
#endif
		}

		if (FFI_G(symbols)) {
			ZEND_HASH_FOREACH_STR_KEY_PTR(FFI_G(symbols), name, sym) {
				if (sym->kind == ZEND_FFI_SYM_VAR) {
					addr = DL_FETCH_SYMBOL(handle, ZSTR_VAL(name));
					if (!addr) {
						zend_error(E_WARNING, "FFI: failed pre-loading '%s', cannot resolve variable '%s'", filename, ZSTR_VAL(name));
						if (lib) {
							DL_UNLOAD(handle);
						}
						goto cleanup;
					}
					sym->addr = addr;
				} else if (sym->kind == ZEND_FFI_SYM_FUNC) {
					addr = DL_FETCH_SYMBOL(handle, ZSTR_VAL(name));
					if (!addr) {
						zend_error(E_WARNING, "FFI: failed pre-loading '%s', cannot resolve function '%s'", filename, ZSTR_VAL(name));
						if (lib) {
							DL_UNLOAD(handle);
						}
						goto cleanup;
					}
					sym->addr = addr;
				}
				if (scope && scope->symbols) {
					zend_ffi_symbol *old_sym = zend_hash_find_ptr(scope->symbols, name);

					if (old_sym) {
						if (zend_ffi_same_symbols(old_sym, sym)) {
							if (ZEND_FFI_TYPE_IS_OWNED(sym->type)
							 && ZEND_FFI_TYPE(old_sym->type) != ZEND_FFI_TYPE(sym->type)) {
								zend_ffi_type *type = ZEND_FFI_TYPE(sym->type);
								zend_ffi_cleanup_type(ZEND_FFI_TYPE(old_sym->type), ZEND_FFI_TYPE(type));
								zend_ffi_type_dtor(type);
							}
						} else {
							zend_error(E_WARNING, "FFI: failed pre-loading '%s', redefinition of '%s'", filename, ZSTR_VAL(name));
							if (lib) {
								DL_UNLOAD(handle);
							}
							goto cleanup;
						}
					}
				}
			} ZEND_HASH_FOREACH_END();
		}

		if (FFI_G(tags)) {
			zend_ffi_tag *tag;

			ZEND_HASH_FOREACH_STR_KEY_PTR(FFI_G(tags), name, tag) {
				if (scope && scope->tags) {
					zend_ffi_tag *old_tag = zend_hash_find_ptr(scope->tags, name);

					if (old_tag) {
						if (zend_ffi_same_tags(old_tag, tag)) {
							if (ZEND_FFI_TYPE_IS_OWNED(tag->type)
							 && ZEND_FFI_TYPE(old_tag->type) != ZEND_FFI_TYPE(tag->type)) {
								zend_ffi_type *type = ZEND_FFI_TYPE(tag->type);
								zend_ffi_cleanup_type(ZEND_FFI_TYPE(old_tag->type), ZEND_FFI_TYPE(type));
								zend_ffi_type_dtor(type);
							}
						} else {
							zend_error(E_WARNING, "FFI: failed pre-loading '%s', redefinition of '%s %s'", filename, zend_ffi_tag_kind_name[tag->kind], ZSTR_VAL(name));
							if (lib) {
								DL_UNLOAD(handle);
							}
							goto cleanup;
						}
					}
				}
			} ZEND_HASH_FOREACH_END();
		}

		if (!scope) {
			scope = malloc(sizeof(zend_ffi_scope));
			scope->symbols = FFI_G(symbols);
			scope->tags = FFI_G(tags);

			if (!FFI_G(scopes)) {
				FFI_G(scopes) = malloc(sizeof(HashTable));
				zend_hash_init(FFI_G(scopes), 0, NULL, zend_ffi_scope_hash_dtor, 1);
			}

			zend_hash_str_add_ptr(FFI_G(scopes), scope_name, scope_name_len, scope);

			FFI_G(symbols) = NULL;
			FFI_G(tags) = NULL;
		} else {
			if (FFI_G(symbols)) {
				if (!scope->symbols) {
					scope->symbols = FFI_G(symbols);
					FFI_G(symbols) = NULL;
				} else {
					ZEND_HASH_FOREACH_STR_KEY_PTR(FFI_G(symbols), name, sym) {
						if (!zend_hash_add_ptr(scope->symbols, name, sym)) {
							zend_ffi_type_dtor(sym->type);
							free(sym);
						}
					} ZEND_HASH_FOREACH_END();
					FFI_G(symbols)->pDestructor = NULL;
				}
			}
			if (FFI_G(tags)) {
				if (!scope->tags) {
					scope->tags = FFI_G(tags);
					FFI_G(tags) = NULL;
				} else {
					ZEND_HASH_FOREACH_STR_KEY_PTR(FFI_G(tags), name, tag) {
						if (!zend_hash_add_ptr(scope->tags, name, tag)) {
							zend_ffi_type_dtor(tag->type);
							free(tag);
						}
					} ZEND_HASH_FOREACH_END();
					FFI_G(tags)->pDestructor = NULL;
				}
			}
		}
	}

cleanup:
	efree(code);
	if (FFI_G(symbols)) {
		zend_hash_destroy(FFI_G(symbols));
		free(FFI_G(symbols));
		FFI_G(symbols) = NULL;
	}
	if (FFI_G(tags)) {
		zend_hash_destroy(FFI_G(tags));
		free(FFI_G(tags));
		FFI_G(tags) = NULL;
	}
} /* }}} */

static int zend_ffi_post_startup(void) /* {{{ */
{
	char *buf, *path, *end;

	FFI_G(persistent) = 1;

	/* Preload list of files and directories specified in FFI_G(preload)*/
	buf = estrdup(FFI_G(preload));
	for (path = buf; path; path=end) {
		end = strchr(path, DEFAULT_DIR_SEPARATOR);
		if (end) {
			*(end++) = 0;
		}
		if (strlen(path) != 0) {
			zend_ffi_preload(path);
		}
	}
	efree(buf);

	FFI_G(persistent) = 0;

	if (orig_post_startup_cb) {
		return orig_post_startup_cb();
	} else {
		return SUCCESS;
	}
}
/* }}} */

static ZEND_COLD zend_function *zend_fake_get_constructor(zend_object *object) /* {{{ */
{
	zend_throw_error(NULL, "Instantiation of '%s' is not allowed", ZSTR_VAL(object->ce->name));
	return NULL;
}
/* }}} */

static ZEND_COLD zend_never_inline void zend_bad_array_access(zend_class_entry *ce) /* {{{ */
{
	zend_throw_error(NULL, "Cannot use object of type %s as array", ZSTR_VAL(ce->name));
}
/* }}} */

static ZEND_COLD zval *zend_fake_read_dimension(zval *object, zval *offset, int type, zval *rv) /* {{{ */
{
	zend_class_entry *ce = Z_OBJCE_P(object);
    zend_bad_array_access(ce);
	return NULL;
}
/* }}} */

static ZEND_COLD void zend_fake_write_dimension(zval *object, zval *offset, zval *value) /* {{{ */
{
	zend_class_entry *ce = Z_OBJCE_P(object);
    zend_bad_array_access(ce);
}
/* }}} */

static ZEND_COLD int zend_fake_has_dimension(zval *object, zval *offset, int check_empty) /* {{{ */
{
	zend_class_entry *ce = Z_OBJCE_P(object);
    zend_bad_array_access(ce);
	return 0;
}
/* }}} */

static ZEND_COLD void zend_fake_unset_dimension(zval *object, zval *offset) /* {{{ */
{
	zend_class_entry *ce = Z_OBJCE_P(object);
    zend_bad_array_access(ce);
}
/* }}} */

static ZEND_COLD zend_never_inline void zend_bad_property_access(zend_class_entry *ce) /* {{{ */
{
	zend_throw_error(NULL, "Cannot access property of object of type %s", ZSTR_VAL(ce->name));
}
/* }}} */

static ZEND_COLD zval *zend_fake_read_property(zval *object, zval *member, int type, void **cache_slot, zval *rv) /* {{{ */
{
	zend_class_entry *ce = Z_OBJCE_P(object);
    zend_bad_property_access(ce);
	return NULL;
}
/* }}} */

static ZEND_COLD void zend_fake_write_property(zval *object, zval *member, zval *value, void **cache_slot) /* {{{ */
{
	zend_class_entry *ce = Z_OBJCE_P(object);
    zend_bad_array_access(ce);
}
/* }}} */

static ZEND_COLD int zend_fake_has_property(zval *object, zval *member, int has_set_exists, void **cache_slot) /* {{{ */
{
	zend_class_entry *ce = Z_OBJCE_P(object);
    zend_bad_array_access(ce);
	return 0;
}
/* }}} */

static ZEND_COLD void zend_fake_unset_property(zval *object, zval *member, void **cache_slot) /* {{{ */
{
	zend_class_entry *ce = Z_OBJCE_P(object);
    zend_bad_array_access(ce);
}
/* }}} */

static zval *zend_fake_get_property_ptr_ptr(zval *object, zval *member, int type, void **cache_slot) /* {{{ */
{
	return NULL;
}
/* }}} */

static ZEND_COLD zend_function *zend_fake_get_method(zend_object **obj_ptr, zend_string *method_name, const zval *key) /* {{{ */
{
	zend_class_entry *ce = (*obj_ptr)->ce;
	zend_throw_error(NULL, "Object of type %s does not support method calls", ZSTR_VAL(ce->name));
	return NULL;
}
/* }}} */

/* {{{ ZEND_MINIT_FUNCTION
 */
ZEND_MINIT_FUNCTION(ffi)
{
	zend_class_entry ce;

	REGISTER_INI_ENTRIES();

	INIT_NS_CLASS_ENTRY(ce, "FFI", "Exception", NULL);
	zend_ffi_exception_ce = zend_register_internal_class_ex(&ce, zend_ce_error);

	INIT_NS_CLASS_ENTRY(ce, "FFI", "ParserException", NULL);
	zend_ffi_parser_exception_ce = zend_register_internal_class_ex(&ce, zend_ffi_exception_ce);
	zend_ffi_parser_exception_ce->ce_flags |= ZEND_ACC_FINAL;

	INIT_CLASS_ENTRY(ce, "FFI", zend_ffi_functions);
	zend_ffi_ce = zend_register_internal_class(&ce);
	zend_ffi_ce->ce_flags |= ZEND_ACC_FINAL;
	zend_ffi_ce->create_object = zend_ffi_new;
	zend_ffi_ce->serialize = zend_class_serialize_deny;
	zend_ffi_ce->unserialize = zend_class_unserialize_deny;

	memcpy(&zend_ffi_new_fn, zend_hash_str_find_ptr(&zend_ffi_ce->function_table, "new", sizeof("new")-1), sizeof(zend_internal_function));
	zend_ffi_new_fn.fn_flags &= ~ZEND_ACC_STATIC;
	memcpy(&zend_ffi_cast_fn, zend_hash_str_find_ptr(&zend_ffi_ce->function_table, "cast", sizeof("cast")-1), sizeof(zend_internal_function));
	zend_ffi_cast_fn.fn_flags &= ~ZEND_ACC_STATIC;
	memcpy(&zend_ffi_type_fn, zend_hash_str_find_ptr(&zend_ffi_ce->function_table, "type", sizeof("type")-1), sizeof(zend_internal_function));
	zend_ffi_type_fn.fn_flags &= ~ZEND_ACC_STATIC;

	memcpy(&zend_ffi_handlers, zend_get_std_object_handlers(), sizeof(zend_object_handlers));
	zend_ffi_handlers.free_obj             = zend_ffi_free_obj;
	zend_ffi_handlers.clone_obj            = NULL;
	zend_ffi_handlers.read_property        = zend_ffi_read_var;
	zend_ffi_handlers.write_property       = zend_ffi_write_var;
	zend_ffi_handlers.read_dimension       = zend_fake_read_dimension;
	zend_ffi_handlers.write_dimension      = zend_fake_write_dimension;
	zend_ffi_handlers.get_property_ptr_ptr = zend_fake_get_property_ptr_ptr;
	zend_ffi_handlers.has_property         = zend_fake_has_property;
	zend_ffi_handlers.unset_property       = zend_fake_unset_property;
	zend_ffi_handlers.has_dimension        = zend_fake_has_dimension;
	zend_ffi_handlers.unset_dimension      = zend_fake_unset_dimension;
	zend_ffi_handlers.get_method           = zend_ffi_get_func;
	zend_ffi_handlers.compare_objects      = NULL;
	zend_ffi_handlers.cast_object          = NULL;
	zend_ffi_handlers.get_debug_info       = NULL;
	zend_ffi_handlers.get_closure          = NULL;

	INIT_NS_CLASS_ENTRY(ce, "FFI", "CData", NULL);
	zend_ffi_cdata_ce = zend_register_internal_class(&ce);
	zend_ffi_cdata_ce->ce_flags |= ZEND_ACC_FINAL;
	zend_ffi_cdata_ce->create_object = zend_ffi_cdata_new;
	zend_ffi_cdata_ce->get_iterator = zend_ffi_cdata_get_iterator;
	zend_ffi_cdata_ce->serialize = zend_class_serialize_deny;
	zend_ffi_cdata_ce->unserialize = zend_class_unserialize_deny;

	memcpy(&zend_ffi_cdata_handlers, zend_get_std_object_handlers(), sizeof(zend_object_handlers));
	zend_ffi_cdata_handlers.get_constructor      = zend_fake_get_constructor;
	zend_ffi_cdata_handlers.free_obj             = zend_ffi_cdata_free_obj;
	zend_ffi_cdata_handlers.clone_obj            = zend_ffi_cdata_clone_obj;
	zend_ffi_cdata_handlers.read_property        = zend_ffi_cdata_read_field;
	zend_ffi_cdata_handlers.write_property       = zend_ffi_cdata_write_field;
	zend_ffi_cdata_handlers.read_dimension       = zend_ffi_cdata_read_dim;
	zend_ffi_cdata_handlers.write_dimension      = zend_ffi_cdata_write_dim;
	zend_ffi_cdata_handlers.get_property_ptr_ptr = zend_fake_get_property_ptr_ptr;
	zend_ffi_cdata_handlers.has_property         = zend_fake_has_property;
	zend_ffi_cdata_handlers.unset_property       = zend_fake_unset_property;
	zend_ffi_cdata_handlers.has_dimension        = zend_fake_has_dimension;
	zend_ffi_cdata_handlers.unset_dimension      = zend_fake_unset_dimension;
	zend_ffi_cdata_handlers.get_method           = zend_fake_get_method;
	zend_ffi_cdata_handlers.get_class_name       = zend_ffi_cdata_get_class_name;
	zend_ffi_cdata_handlers.compare_objects      = zend_ffi_cdata_compare_objects;
	zend_ffi_cdata_handlers.cast_object          = zend_ffi_cdata_cast_object;
	zend_ffi_cdata_handlers.count_elements       = zend_ffi_cdata_count_elements;
	zend_ffi_cdata_handlers.get_debug_info       = zend_ffi_cdata_get_debug_info;
	zend_ffi_cdata_handlers.get_closure          = zend_ffi_cdata_get_closure;

	memcpy(&zend_ffi_cdata_value_handlers, zend_get_std_object_handlers(), sizeof(zend_object_handlers));
	zend_ffi_cdata_value_handlers.get_constructor      = zend_fake_get_constructor;
	zend_ffi_cdata_value_handlers.free_obj             = zend_ffi_cdata_free_obj;
	zend_ffi_cdata_value_handlers.clone_obj            = zend_ffi_cdata_clone_obj;
	zend_ffi_cdata_value_handlers.read_property        = zend_fake_read_property;
	zend_ffi_cdata_value_handlers.write_property       = zend_fake_write_property;
	zend_ffi_cdata_value_handlers.read_dimension       = zend_fake_read_dimension;
	zend_ffi_cdata_value_handlers.write_dimension      = zend_fake_write_dimension;
	zend_ffi_cdata_value_handlers.get_property_ptr_ptr = zend_fake_get_property_ptr_ptr;
	zend_ffi_cdata_value_handlers.get                  = zend_ffi_cdata_get;
	zend_ffi_cdata_value_handlers.set                  = zend_ffi_cdata_set;
	zend_ffi_cdata_value_handlers.has_property         = zend_fake_has_property;
	zend_ffi_cdata_value_handlers.unset_property       = zend_fake_unset_property;
	zend_ffi_cdata_value_handlers.has_dimension        = zend_fake_has_dimension;
	zend_ffi_cdata_value_handlers.unset_dimension      = zend_fake_unset_dimension;
	zend_ffi_cdata_value_handlers.get_method           = zend_fake_get_method;
	zend_ffi_cdata_value_handlers.get_class_name       = zend_ffi_cdata_get_class_name;
	zend_ffi_cdata_value_handlers.compare_objects      = zend_ffi_cdata_compare_objects;
	zend_ffi_cdata_value_handlers.cast_object          = NULL;
	zend_ffi_cdata_value_handlers.count_elements       = NULL;
	zend_ffi_cdata_value_handlers.get_debug_info       = zend_ffi_cdata_get_debug_info;
	zend_ffi_cdata_value_handlers.get_closure          = NULL;

	INIT_NS_CLASS_ENTRY(ce, "FFI", "CType", NULL);
	zend_ffi_ctype_ce = zend_register_internal_class(&ce);
	zend_ffi_ctype_ce->ce_flags |= ZEND_ACC_FINAL;
	zend_ffi_ctype_ce->create_object = zend_ffi_ctype_new;
	zend_ffi_ctype_ce->serialize = zend_class_serialize_deny;
	zend_ffi_ctype_ce->unserialize = zend_class_unserialize_deny;

	memcpy(&zend_ffi_ctype_handlers, zend_get_std_object_handlers(), sizeof(zend_object_handlers));
	zend_ffi_ctype_handlers.get_constructor      = zend_fake_get_constructor;
	zend_ffi_ctype_handlers.free_obj             = zend_ffi_ctype_free_obj;
	zend_ffi_ctype_handlers.clone_obj            = NULL;
	zend_ffi_ctype_handlers.read_property        = zend_fake_read_property;
	zend_ffi_ctype_handlers.write_property       = zend_fake_write_property;
	zend_ffi_ctype_handlers.read_dimension       = zend_fake_read_dimension;
	zend_ffi_ctype_handlers.write_dimension      = zend_fake_write_dimension;
	zend_ffi_ctype_handlers.get_property_ptr_ptr = zend_fake_get_property_ptr_ptr;
	zend_ffi_ctype_handlers.has_property         = zend_fake_has_property;
	zend_ffi_ctype_handlers.unset_property       = zend_fake_unset_property;
	zend_ffi_ctype_handlers.has_dimension        = zend_fake_has_dimension;
	zend_ffi_ctype_handlers.unset_dimension      = zend_fake_unset_dimension;
	zend_ffi_ctype_handlers.get_method           = zend_fake_get_method;
	zend_ffi_ctype_handlers.get_class_name       = zend_ffi_ctype_get_class_name;
	zend_ffi_ctype_handlers.compare_objects      = zend_ffi_ctype_compare_objects;
	zend_ffi_ctype_handlers.cast_object          = NULL;
	zend_ffi_ctype_handlers.count_elements       = NULL;
	zend_ffi_ctype_handlers.get_debug_info       = zend_ffi_ctype_get_debug_info;
	zend_ffi_ctype_handlers.get_closure          = NULL;

	if (FFI_G(preload)) {
		orig_post_startup_cb = zend_post_startup_cb;
		zend_post_startup_cb = zend_ffi_post_startup;
	}

	return SUCCESS;
}
/* }}} */

/* {{{ ZEND_RSHUTDOWN_FUNCTION
 */
ZEND_RSHUTDOWN_FUNCTION(ffi)
{
	if (FFI_G(callbacks)) {
		zend_hash_destroy(FFI_G(callbacks));
		efree(FFI_G(callbacks));
		FFI_G(callbacks) = NULL;
	}
	return SUCCESS;
}
/* }}} */

/* {{{ ZEND_MINFO_FUNCTION
 */
ZEND_MINFO_FUNCTION(ffi)
{
	php_info_print_table_start();
	php_info_print_table_header(2, "FFI support", "enabled");
	php_info_print_table_end();

	DISPLAY_INI_ENTRIES();
}
/* }}} */

#define ZEND_FFI_VERSION "0.1.0"

static const zend_ffi_type zend_ffi_type_void = {.kind=ZEND_FFI_TYPE_VOID, .size=1, .align=1};
static const zend_ffi_type zend_ffi_type_char = {.kind=ZEND_FFI_TYPE_CHAR, .size=1, .align=_Alignof(char)};
static const zend_ffi_type zend_ffi_type_bool = {.kind=ZEND_FFI_TYPE_BOOL, .size=1, .align=_Alignof(uint8_t)};
static const zend_ffi_type zend_ffi_type_sint8 = {.kind=ZEND_FFI_TYPE_SINT8, .size=1, .align=_Alignof(int8_t)};
static const zend_ffi_type zend_ffi_type_uint8 = {.kind=ZEND_FFI_TYPE_UINT8, .size=1, .align=_Alignof(uint8_t)};
static const zend_ffi_type zend_ffi_type_sint16 = {.kind=ZEND_FFI_TYPE_SINT16, .size=2, .align=_Alignof(int16_t)};
static const zend_ffi_type zend_ffi_type_uint16 = {.kind=ZEND_FFI_TYPE_UINT16, .size=2, .align=_Alignof(uint16_t)};
static const zend_ffi_type zend_ffi_type_sint32 = {.kind=ZEND_FFI_TYPE_SINT32, .size=4, .align=_Alignof(int32_t)};
static const zend_ffi_type zend_ffi_type_uint32 = {.kind=ZEND_FFI_TYPE_UINT32, .size=4, .align=_Alignof(uint32_t)};
static const zend_ffi_type zend_ffi_type_sint64 = {.kind=ZEND_FFI_TYPE_SINT64, .size=8, .align=_Alignof(int64_t)};
static const zend_ffi_type zend_ffi_type_uint64 = {.kind=ZEND_FFI_TYPE_UINT64, .size=8, .align=_Alignof(uint64_t)};
static const zend_ffi_type zend_ffi_type_float = {.kind=ZEND_FFI_TYPE_FLOAT, .size=sizeof(float), .align=_Alignof(float)};
static const zend_ffi_type zend_ffi_type_double = {.kind=ZEND_FFI_TYPE_DOUBLE, .size=sizeof(double), .align=_Alignof(double)};

#ifdef HAVE_LONG_DOUBLE
static const zend_ffi_type zend_ffi_type_long_double = {.kind=ZEND_FFI_TYPE_LONGDOUBLE, .size=sizeof(long double), .align=_Alignof(long double)};
#endif

static const zend_ffi_type zend_ffi_type_ptr = {.kind=ZEND_FFI_TYPE_POINTER, .size=sizeof(void*), .align=_Alignof(void*), .pointer.type = (zend_ffi_type*)&zend_ffi_type_void};

const struct {
	const char *name;
	const zend_ffi_type *type;
} zend_ffi_types[] = {
	{"void",        &zend_ffi_type_void},
	{"char",        &zend_ffi_type_char},
	{"bool",        &zend_ffi_type_bool},
	{"int8_t",      &zend_ffi_type_sint8},
	{"uint8_t",     &zend_ffi_type_uint8},
	{"int16_t",     &zend_ffi_type_sint16},
	{"uint16_t",    &zend_ffi_type_uint16},
	{"int32_t",     &zend_ffi_type_sint32},
	{"uint32_t",    &zend_ffi_type_uint32},
	{"int64_t",     &zend_ffi_type_sint64},
	{"uint64_t",    &zend_ffi_type_uint64},
	{"float",       &zend_ffi_type_float},
	{"double",      &zend_ffi_type_double},
#ifdef HAVE_LONG_DOUBLE
	{"long double", &zend_ffi_type_long_double},
#endif
#if SIZEOF_SIZE_T == 4
	{"uintptr_t",  &zend_ffi_type_uint32},
	{"intptr_t",   &zend_ffi_type_sint32},
	{"size_t",     &zend_ffi_type_uint32},
	{"ssize_t",    &zend_ffi_type_sint32},
	{"ptrdiff_t",  &zend_ffi_type_sint32},
#else
	{"uintptr_t",  &zend_ffi_type_uint64},
	{"intptr_t",   &zend_ffi_type_sint64},
	{"size_t",     &zend_ffi_type_uint64},
	{"ssize_t",    &zend_ffi_type_sint64},
	{"ptrdiff_t",  &zend_ffi_type_sint64},
#endif
#if SIZEOF_OFF_T == 4
	{"off_t",      &zend_ffi_type_sint32},
#else
	{"off_t",      &zend_ffi_type_sint64},
#endif

	{"va_list",           &zend_ffi_type_ptr},
	{"__builtin_va_list", &zend_ffi_type_ptr},
	{"__gnuc_va_list",    &zend_ffi_type_ptr},
};

/* {{{ ZEND_GINIT_FUNCTION
 */
static ZEND_GINIT_FUNCTION(ffi)
{
	size_t i;

#if defined(COMPILE_DL_FFI) && defined(ZTS)
	ZEND_TSRMLS_CACHE_UPDATE();
#endif
	memset(ffi_globals, 0, sizeof(*ffi_globals));
	zend_hash_init(&ffi_globals->types, 0, NULL, NULL, 1);
	for (i = 0; i < sizeof(zend_ffi_types)/sizeof(zend_ffi_types[0]); i++) {
		zend_hash_str_add_new_ptr(&ffi_globals->types, zend_ffi_types[i].name, strlen(zend_ffi_types[i].name), (void*)zend_ffi_types[i].type);
	}
}
/* }}} */

/* {{{ ZEND_GINIT_FUNCTION
 */
static ZEND_GSHUTDOWN_FUNCTION(ffi)
{
	if (ffi_globals->scopes) {
		zend_hash_destroy(ffi_globals->scopes);
		free(ffi_globals->scopes);
	}
	zend_hash_destroy(&ffi_globals->types);
}
/* }}} */

/* {{{ ffi_module_entry
 */
zend_module_entry ffi_module_entry = {
	STANDARD_MODULE_HEADER,
	"FFI",					/* Extension name */
	NULL,					/* zend_function_entry */
	ZEND_MINIT(ffi),		/* ZEND_MINIT - Module initialization */
	NULL,					/* ZEND_MSHUTDOWN - Module shutdown */
	NULL,					/* ZEND_RINIT - Request initialization */
	ZEND_RSHUTDOWN(ffi),	/* ZEND_RSHUTDOWN - Request shutdown */
	ZEND_MINFO(ffi),		/* ZEND_MINFO - Module info */
	ZEND_FFI_VERSION,		/* Version */
	ZEND_MODULE_GLOBALS(ffi),
	ZEND_GINIT(ffi),
	ZEND_GSHUTDOWN(ffi),
	NULL,
	STANDARD_MODULE_PROPERTIES_EX
};
/* }}} */

#ifdef COMPILE_DL_FFI
# ifdef ZTS
ZEND_TSRMLS_CACHE_DEFINE()
# endif
ZEND_GET_MODULE(ffi)
#endif

/* parser callbacks */
void zend_ffi_parser_error(const char *format, ...) /* {{{ */
{
	va_list va;
	char *message = NULL;

	va_start(va, format);
	zend_vspprintf(&message, 0, format, va);

	if (EG(current_execute_data)) {
		zend_throw_exception(zend_ffi_parser_exception_ce, message, 0);
	} else {
		zend_error(E_WARNING, "FFI Parser: %s", message);
	}

	efree(message);
	va_end(va);

	LONGJMP(FFI_G(bailout), FAILURE);
}
/* }}} */

static void zend_ffi_finalize_type(zend_ffi_dcl *dcl) /* {{{ */
{
	if (!dcl->type) {
		switch (dcl->flags & ZEND_FFI_DCL_TYPE_SPECIFIERS) {
			case ZEND_FFI_DCL_VOID:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_void;
				break;
			case ZEND_FFI_DCL_CHAR:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_char;
				break;
			case ZEND_FFI_DCL_CHAR|ZEND_FFI_DCL_SIGNED:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_sint8;
				break;
			case ZEND_FFI_DCL_CHAR|ZEND_FFI_DCL_UNSIGNED:
			case ZEND_FFI_DCL_BOOL:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_uint8;
				break;
			case ZEND_FFI_DCL_SHORT:
			case ZEND_FFI_DCL_SHORT|ZEND_FFI_DCL_SIGNED:
			case ZEND_FFI_DCL_SHORT|ZEND_FFI_DCL_INT:
			case ZEND_FFI_DCL_SHORT|ZEND_FFI_DCL_SIGNED|ZEND_FFI_DCL_INT:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_sint16;
				break;
			case ZEND_FFI_DCL_SHORT|ZEND_FFI_DCL_UNSIGNED:
			case ZEND_FFI_DCL_SHORT|ZEND_FFI_DCL_UNSIGNED|ZEND_FFI_DCL_INT:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_uint16;
				break;
			case ZEND_FFI_DCL_INT:
			case ZEND_FFI_DCL_SIGNED:
			case ZEND_FFI_DCL_SIGNED|ZEND_FFI_DCL_INT:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_sint32;
				break;
			case ZEND_FFI_DCL_UNSIGNED:
			case ZEND_FFI_DCL_UNSIGNED|ZEND_FFI_DCL_INT:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_uint32;
				break;
			case ZEND_FFI_DCL_LONG:
			case ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_SIGNED:
			case ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_INT:
			case ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_SIGNED|ZEND_FFI_DCL_INT:
				if (sizeof(long) == 4) {
					dcl->type = (zend_ffi_type*)&zend_ffi_type_sint32;
				} else {
					dcl->type = (zend_ffi_type*)&zend_ffi_type_sint64;
				}
				break;
			case ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_UNSIGNED:
			case ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_UNSIGNED|ZEND_FFI_DCL_INT:
				if (sizeof(long) == 4) {
					dcl->type = (zend_ffi_type*)&zend_ffi_type_uint32;
				} else {
					dcl->type = (zend_ffi_type*)&zend_ffi_type_uint64;
				}
				break;
			case ZEND_FFI_DCL_LONG_LONG|ZEND_FFI_DCL_LONG:
			case ZEND_FFI_DCL_LONG_LONG|ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_SIGNED:
			case ZEND_FFI_DCL_LONG_LONG|ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_INT:
			case ZEND_FFI_DCL_LONG_LONG|ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_SIGNED|ZEND_FFI_DCL_INT:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_sint64;
				break;
			case ZEND_FFI_DCL_LONG_LONG|ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_UNSIGNED:
			case ZEND_FFI_DCL_LONG_LONG|ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_UNSIGNED|ZEND_FFI_DCL_INT:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_uint64;
				break;
			case ZEND_FFI_DCL_FLOAT:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_float;
				break;
			case ZEND_FFI_DCL_DOUBLE:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_double;
				break;
			case ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_DOUBLE:
				dcl->type = (zend_ffi_type*)&zend_ffi_type_long_double;
				break;
			case ZEND_FFI_DCL_FLOAT|ZEND_FFI_DCL_COMPLEX:
			case ZEND_FFI_DCL_DOUBLE|ZEND_FFI_DCL_COMPLEX:
			case ZEND_FFI_DCL_DOUBLE|ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_COMPLEX:
				zend_ffi_parser_error("unsupported type '_Complex' at line %d", FFI_G(line));
				break;
			default:
				zend_ffi_parser_error("unsupported type specifier combination at line %d", FFI_G(line));
				break;
		}
		dcl->flags &= ~ZEND_FFI_DCL_TYPE_SPECIFIERS;
		dcl->flags |= ZEND_FFI_DCL_TYPEDEF_NAME;
	}
}
/* }}} */

int zend_ffi_is_typedef_name(const char *name, size_t name_len) /* {{{ */
{
	zend_ffi_symbol *sym;
	zend_ffi_type *type;

	if (FFI_G(symbols)) {
		sym = zend_hash_str_find_ptr(FFI_G(symbols), name, name_len);
		if (sym) {
			return (sym->kind == ZEND_FFI_SYM_TYPE);
		}
	}
	type = zend_hash_str_find_ptr(&FFI_G(types), name, name_len);
	if (type) {
		return 1;
	}
	return 0;
}
/* }}} */

void zend_ffi_resolve_typedef(const char *name, size_t name_len, zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_symbol *sym;
	zend_ffi_type *type;

	if (FFI_G(symbols)) {
		sym = zend_hash_str_find_ptr(FFI_G(symbols), name, name_len);
		if (sym && sym->kind == ZEND_FFI_SYM_TYPE) {
			dcl->type = ZEND_FFI_TYPE(sym->type);;
			if (sym->is_const) {
				dcl->attr |= ZEND_FFI_ATTR_CONST;
			}
			return;
		}
	}
	type = zend_hash_str_find_ptr(&FFI_G(types), name, name_len);
	if (type) {
		dcl->type = type;
		return;
	}
	zend_ffi_parser_error("undefined C type '%.*s' at line %d", name_len, name, FFI_G(line));
}
/* }}} */

void zend_ffi_resolve_const(const char *name, size_t name_len, zend_ffi_val *val) /* {{{ */
{
	zend_ffi_symbol *sym;

	if (FFI_G(symbols)) {
		sym = zend_hash_str_find_ptr(FFI_G(symbols), name, name_len);
		if (sym && sym->kind == ZEND_FFI_SYM_CONST) {
			val->i64 = sym->value;
			switch (sym->type->kind) {
				case ZEND_FFI_TYPE_SINT8:
				case ZEND_FFI_TYPE_SINT16:
				case ZEND_FFI_TYPE_SINT32:
					val->kind = ZEND_FFI_VAL_INT32;
					break;
				case ZEND_FFI_TYPE_SINT64:
					val->kind = ZEND_FFI_VAL_INT64;
					break;
				case ZEND_FFI_TYPE_UINT8:
				case ZEND_FFI_TYPE_UINT16:
				case ZEND_FFI_TYPE_UINT32:
					val->kind = ZEND_FFI_VAL_UINT32;
					break;
				case ZEND_FFI_TYPE_UINT64:
					val->kind = ZEND_FFI_VAL_UINT64;
					break;
				default:
					ZEND_ASSERT(0);
			}
			return;
		}
	}
	val->kind = ZEND_FFI_VAL_ERROR;
}
/* }}} */

void zend_ffi_make_enum_type(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_type *type = pemalloc(sizeof(zend_ffi_type), FFI_G(persistent));
	type->kind = ZEND_FFI_TYPE_ENUM;
	type->attr = (dcl->attr & ZEND_FFI_ENUM_ATTRS);
	if (FFI_G(persistent)) {
		type->attr |= ZEND_FFI_ATTR_PERSISTENT;
	}
	if (type->attr & ZEND_FFI_ATTR_PACKED) {
		type->size = zend_ffi_type_uint8.size;
		type->align = zend_ffi_type_uint8.align;
		type->enumeration.kind = ZEND_FFI_TYPE_UINT8;
	} else {
		type->size = zend_ffi_type_uint32.size;
		type->align = zend_ffi_type_uint32.align;
		type->enumeration.kind = ZEND_FFI_TYPE_UINT32;
	}
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
	dcl->attr &= ~ZEND_FFI_ENUM_ATTRS;
}
/* }}} */

void zend_ffi_add_enum_val(zend_ffi_dcl *enum_dcl, const char *name, size_t name_len, zend_ffi_val *val, int64_t *min, int64_t *max, int64_t *last) /* {{{ */
{
	zend_ffi_symbol *sym;
	const zend_ffi_type *sym_type;
	int64_t value;
	zend_ffi_type *enum_type = ZEND_FFI_TYPE(enum_dcl->type);
	zend_bool overflow = 0;
	zend_bool is_signed =
		(enum_type->enumeration.kind == ZEND_FFI_TYPE_SINT8 ||
		 enum_type->enumeration.kind == ZEND_FFI_TYPE_SINT16 ||
		 enum_type->enumeration.kind == ZEND_FFI_TYPE_SINT32 ||
		 enum_type->enumeration.kind == ZEND_FFI_TYPE_SINT64);

	ZEND_ASSERT(enum_type && enum_type->kind == ZEND_FFI_TYPE_ENUM);
	if (val->kind == ZEND_FFI_VAL_EMPTY) {
		if (is_signed) {
			if (*last == 0x7FFFFFFFFFFFFFFFLL) {
				overflow = 1;
			}
		} else {
			if ((*min != 0 || *max != 0)
			 && (uint64_t)*last == 0xFFFFFFFFFFFFFFFFULL) {
				overflow = 1;
			}
		}
		value = *last + 1;
	} else if (val->kind == ZEND_FFI_VAL_CHAR) {
		if (!is_signed && val->ch < 0) {
			if ((uint64_t)*max > 0x7FFFFFFFFFFFFFFFULL) {
				overflow = 1;
			} else {
				is_signed = 1;
			}
		}
		value = val->ch;
	} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
		if (!is_signed && val->i64 < 0) {
			if ((uint64_t)*max > 0x7FFFFFFFFFFFFFFFULL) {
				overflow = 1;
			} else {
				is_signed = 1;
			}
		}
		value = val->i64;
	} else if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) {
		if (is_signed && val->u64 > 0x7FFFFFFFFFFFFFFFULL) {
			overflow = 1;
		}
		value = val->u64;
	} else {
		zend_ffi_parser_error("enumerator value '%.*s' must be an integer at line %d", name_len, name, FFI_G(line));
		return;
	}

	if (overflow) {
		zend_ffi_parser_error("overflow in enumeration values '%.*s' at line %d", name_len, name, FFI_G(line));
		return;
	}

	if (is_signed) {
		*min = MIN(*min, value);
		*max = MAX(*max, value);
		if ((enum_type->attr & ZEND_FFI_ATTR_PACKED)
		 && *min >= -0x7FLL-1 && *max <= 0x7FLL) {
			sym_type = &zend_ffi_type_sint8;
		} else if ((enum_type->attr & ZEND_FFI_ATTR_PACKED)
		 && *min >= -0x7FFFLL-1 && *max <= 0x7FFFLL) {
			sym_type = &zend_ffi_type_sint16;
		} else if (*min >= -0x7FFFFFFFLL-1 && *max <= 0x7FFFFFFFLL) {
			sym_type = &zend_ffi_type_sint32;
		} else {
			sym_type = &zend_ffi_type_sint64;
		}
	} else {
		*min = MIN((uint64_t)*min, (uint64_t)value);
		*max = MAX((uint64_t)*max, (uint64_t)value);
		if ((enum_type->attr & ZEND_FFI_ATTR_PACKED)
		 && (uint64_t)*max <= 0xFFULL) {
			sym_type = &zend_ffi_type_uint8;
		} else if ((enum_type->attr & ZEND_FFI_ATTR_PACKED)
		 && (uint64_t)*max <= 0xFFFFULL) {
			sym_type = &zend_ffi_type_uint16;
		} else if ((uint64_t)*max <= 0xFFFFFFFFULL) {
			sym_type = &zend_ffi_type_uint32;
		} else {
			sym_type = &zend_ffi_type_uint64;
		}
	}
	enum_type->enumeration.kind = sym_type->kind;
	enum_type->size = sym_type->size;
	enum_type->align = sym_type->align;
	*last = value;

	if (!FFI_G(symbols)) {
		FFI_G(symbols) = pemalloc(sizeof(HashTable), FFI_G(persistent));
		zend_hash_init(FFI_G(symbols), 0, NULL, FFI_G(persistent) ? zend_ffi_symbol_hash_persistent_dtor : zend_ffi_symbol_hash_dtor, FFI_G(persistent));
	}
	sym = zend_hash_str_find_ptr(FFI_G(symbols), name, name_len);
	if (sym) {
		zend_ffi_parser_error("redeclaration of '%.*s' at line %d", name_len, name, FFI_G(line));
	} else {
		sym = pemalloc(sizeof(zend_ffi_symbol), FFI_G(persistent));
		sym->kind  = ZEND_FFI_SYM_CONST;
		sym->type  = (zend_ffi_type*)sym_type;
		sym->value = value;
		zend_hash_str_add_new_ptr(FFI_G(symbols), name, name_len, sym);
	}
}
/* }}} */

void zend_ffi_make_struct_type(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_type *type = pemalloc(sizeof(zend_ffi_type), FFI_G(persistent));
	type->kind = ZEND_FFI_TYPE_STRUCT;
	type->attr = (dcl->attr & ZEND_FFI_STRUCT_ATTRS);
	if (FFI_G(persistent)) {
		type->attr |= ZEND_FFI_ATTR_PERSISTENT;
	}
	type->size = 0;
	type->align = dcl->align > 1 ? dcl->align : 1;
	if (dcl->flags & ZEND_FFI_DCL_UNION) {
		type->attr |= ZEND_FFI_ATTR_UNION;
	}
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
	zend_hash_init(&type->record.fields, 0, NULL, FFI_G(persistent) ? zend_ffi_field_hash_persistent_dtor :zend_ffi_field_hash_dtor, FFI_G(persistent));
	dcl->attr &= ~ZEND_FFI_STRUCT_ATTRS;
	dcl->align = 0;
}
/* }}} */

static int zend_ffi_validate_prev_field_type(zend_ffi_type *struct_type) /* {{{ */
{
	if (zend_hash_num_elements(&struct_type->record.fields) > 0) {
		zend_ffi_field *field = NULL;

		ZEND_HASH_REVERSE_FOREACH_PTR(&struct_type->record.fields, field) {
			break;
		} ZEND_HASH_FOREACH_END();
		if (ZEND_FFI_TYPE(field->type)->attr & ZEND_FFI_ATTR_INCOMPLETE_ARRAY) {
			zend_ffi_throw_parser_error("flexible array member not at end of struct at line %d", FFI_G(line));
			return FAILURE;
		}
	}
	return SUCCESS;
}
/* }}} */

static int zend_ffi_validate_field_type(zend_ffi_type *type, zend_ffi_type *struct_type) /* {{{ */
{
	if (type == struct_type) {
		zend_ffi_throw_parser_error("struct/union can't contain an instance of itself at line %d", FFI_G(line));
		return FAILURE;
	} else if (zend_ffi_validate_var_type(type, 1) != SUCCESS) {
		return FAILURE;
	} else if (struct_type->attr & ZEND_FFI_ATTR_UNION) {
		if (type->attr & ZEND_FFI_ATTR_INCOMPLETE_ARRAY) {
			zend_ffi_throw_parser_error("flexible array member in union at line %d", FFI_G(line));
			return FAILURE;
		}
	}
	return zend_ffi_validate_prev_field_type(struct_type);
}
/* }}} */

void zend_ffi_add_field(zend_ffi_dcl *struct_dcl, const char *name, size_t name_len, zend_ffi_dcl *field_dcl) /* {{{ */
{
	zend_ffi_field *field;
	zend_ffi_type *struct_type = ZEND_FFI_TYPE(struct_dcl->type);
	zend_ffi_type *field_type;

	ZEND_ASSERT(struct_type && struct_type->kind == ZEND_FFI_TYPE_STRUCT);
	zend_ffi_finalize_type(field_dcl);
	field_type = ZEND_FFI_TYPE(field_dcl->type);
	if (zend_ffi_validate_field_type(field_type, struct_type) != SUCCESS) {
		zend_ffi_cleanup_dcl(field_dcl);
		LONGJMP(FFI_G(bailout), FAILURE);
	}

	field = pemalloc(sizeof(zend_ffi_field), FFI_G(persistent));
	if (!(struct_type->attr & ZEND_FFI_ATTR_PACKED) && !(field_dcl->attr & ZEND_FFI_ATTR_PACKED)) {
		struct_type->align = MAX(struct_type->align, MAX(field_type->align, field_dcl->align));
	}
	if (struct_type->attr & ZEND_FFI_ATTR_UNION) {
		field->offset = 0;
		struct_type->size = MAX(struct_type->size, field_type->size);
	} else {
		if (!(struct_type->attr & ZEND_FFI_ATTR_PACKED) && !(field_dcl->attr & ZEND_FFI_ATTR_PACKED)) {
			uint32_t field_align = MAX(field_type->align, field_dcl->align);
			struct_type->size = ((struct_type->size + (field_align - 1)) / field_align) * field_align;
		}
		field->offset = struct_type->size;
		struct_type->size += field_type->size;
	}
	field->type = field_dcl->type;
	field->is_const = (field_dcl->attr & ZEND_FFI_ATTR_CONST) != 0;
	field->is_nested = 0;
	field->first_bit = 0;
	field->bits = 0;
	field_dcl->type = field_type; /* reset "owned" flag */

	if (!zend_hash_str_add_ptr(&struct_type->record.fields, name, name_len, field)) {
		zend_ffi_type_dtor(field->type);
		pefree(field, FFI_G(persistent));
		zend_ffi_parser_error("duplicate field name '%.*s' at line %d", name_len, name, FFI_G(line));
	}
}
/* }}} */

void zend_ffi_add_anonymous_field(zend_ffi_dcl *struct_dcl, zend_ffi_dcl *field_dcl) /* {{{ */
{
	zend_ffi_type *struct_type = ZEND_FFI_TYPE(struct_dcl->type);
	zend_ffi_type *field_type;
	zend_ffi_field *field;
	zend_string *key;

	ZEND_ASSERT(struct_type && struct_type->kind == ZEND_FFI_TYPE_STRUCT);
	zend_ffi_finalize_type(field_dcl);
	field_type = ZEND_FFI_TYPE(field_dcl->type);
	if (field_type->kind != ZEND_FFI_TYPE_STRUCT) {
		zend_ffi_cleanup_dcl(field_dcl);
		zend_ffi_parser_error("declaration does not declare anything at line %d", FFI_G(line));
		return;
	}

	if (!(struct_type->attr & ZEND_FFI_ATTR_PACKED) && !(field_dcl->attr & ZEND_FFI_ATTR_PACKED)) {
		struct_type->align = MAX(struct_type->align, MAX(field_type->align, field_dcl->align));
	}
	if (!(struct_type->attr & ZEND_FFI_ATTR_UNION)) {
		if (zend_ffi_validate_prev_field_type(struct_type) != SUCCESS) {
			zend_ffi_cleanup_dcl(field_dcl);
			LONGJMP(FFI_G(bailout), FAILURE);
		}
		if (!(struct_type->attr & ZEND_FFI_ATTR_PACKED) && !(field_dcl->attr & ZEND_FFI_ATTR_PACKED)) {
			uint32_t field_align = MAX(field_type->align, field_dcl->align);
			struct_type->size = ((struct_type->size + (field_align - 1)) / field_align) * field_align;
		}
	}

	ZEND_HASH_FOREACH_STR_KEY_PTR(&field_type->record.fields, key, field) {
		zend_ffi_field *new_field = pemalloc(sizeof(zend_ffi_field), FFI_G(persistent));

		if (struct_type->attr & ZEND_FFI_ATTR_UNION) {
			new_field->offset = field->offset;
		} else {
			new_field->offset = struct_type->size + field->offset;
		}
		new_field->type = field->type;
		new_field->is_const = field->is_const;
		new_field->is_nested = 1;
		new_field->first_bit = field->first_bit;
		new_field->bits = field->bits;
		field->type = ZEND_FFI_TYPE(field->type); /* reset "owned" flag */

		if (key) {
			if (!zend_hash_add_ptr(&struct_type->record.fields, key, new_field)) {
				zend_ffi_type_dtor(new_field->type);
				pefree(new_field, FFI_G(persistent));
				zend_ffi_parser_error("duplicate field name '%s' at line %d", ZSTR_VAL(key), FFI_G(line));
				return;
			}
		} else {
			zend_hash_next_index_insert_ptr(&struct_type->record.fields, field);
		}
	} ZEND_HASH_FOREACH_END();

	if (struct_type->attr & ZEND_FFI_ATTR_UNION) {
		struct_type->size = MAX(struct_type->size, field_type->size);
	} else {
		struct_type->size += field_type->size;
	}

	zend_ffi_type_dtor(field_dcl->type);
	field_dcl->type = NULL;
}
/* }}} */

void zend_ffi_add_bit_field(zend_ffi_dcl *struct_dcl, const char *name, size_t name_len, zend_ffi_dcl *field_dcl, zend_ffi_val *bits) /* {{{ */
{
	zend_ffi_type *struct_type = ZEND_FFI_TYPE(struct_dcl->type);
	zend_ffi_type *field_type;
	zend_ffi_field *field;

	ZEND_ASSERT(struct_type && struct_type->kind == ZEND_FFI_TYPE_STRUCT);
	zend_ffi_finalize_type(field_dcl);
	field_type = ZEND_FFI_TYPE(field_dcl->type);
	if (zend_ffi_validate_field_type(field_type, struct_type) != SUCCESS) {
		zend_ffi_cleanup_dcl(field_dcl);
		LONGJMP(FFI_G(bailout), FAILURE);
	}

	if (field_type->kind < ZEND_FFI_TYPE_UINT8 || field_type->kind > ZEND_FFI_TYPE_BOOL) {
		zend_ffi_cleanup_dcl(field_dcl);
		zend_ffi_parser_error("wrong type of bit field '%.*s' at line %d", name ? name_len : sizeof("<anonymous>")-1, name ? name : "<anonymous>", FFI_G(line));
	}

	if (bits->kind == ZEND_FFI_VAL_INT32 || ZEND_FFI_VAL_INT64) {
		if (bits->i64 < 0) {
			zend_ffi_cleanup_dcl(field_dcl);
			zend_ffi_parser_error("negative width in bit-field '%.*s' at line %d", name ? name_len : sizeof("<anonymous>")-1, name ? name : "<anonymous>", FFI_G(line));
		} else if (bits->i64 == 0) {
			zend_ffi_cleanup_dcl(field_dcl);
			if (name) {
				zend_ffi_parser_error("zero width in bit-field '%.*s' at line %d", name ? name_len : sizeof("<anonymous>")-1, name ? name : "<anonymous>", FFI_G(line));
			}
			return;
		} else if (bits->i64 > field_type->size * 8) {
			zend_ffi_cleanup_dcl(field_dcl);
			zend_ffi_parser_error("width of '%.*s' exceeds its type at line %d", name ? name_len : sizeof("<anonymous>")-1, name ? name : "<anonymous>", FFI_G(line));
		}
	} else if (ZEND_FFI_VAL_UINT32 || ZEND_FFI_VAL_UINT64) {
		if (bits->u64 == 0) {
			zend_ffi_cleanup_dcl(field_dcl);
			if (name) {
				zend_ffi_parser_error("zero width in bit-field '%.*s' at line %d", name ? name_len : sizeof("<anonymous>")-1, name ? name : "<anonymous>", FFI_G(line));
			}
			return;
		} else if (bits->u64 > field_type->size * 8) {
			zend_ffi_cleanup_dcl(field_dcl);
			zend_ffi_parser_error("width of '%.*s' exceeds its type at line %d", name ? name_len : sizeof("<anonymous>")-1, name ? name : "<anonymous>", FFI_G(line));
		}
	} else {
		zend_ffi_cleanup_dcl(field_dcl);
		zend_ffi_parser_error("bit field '%.*s' width not an integer constant at line %d", name ? name_len : sizeof("<anonymous>")-1, name ? name : "<anonymous>", FFI_G(line));
	}

	field = pemalloc(sizeof(zend_ffi_field), FFI_G(persistent));
	if (!(struct_type->attr & ZEND_FFI_ATTR_PACKED)) {
		struct_type->align = MAX(struct_type->align, sizeof(uint32_t));
	}
	if (struct_type->attr & ZEND_FFI_ATTR_UNION) {
		field->offset = 0;
		field->first_bit = 0;
		field->bits = bits->u64;
		if (struct_type->attr & ZEND_FFI_ATTR_PACKED) {
			struct_type->size = MAX(struct_type->size, (bits->u64 + 7) / 8);
		} else {
			struct_type->size = MAX(struct_type->size, ((bits->u64 + 31) / 32) * 4);
		}
	} else {
		zend_ffi_field *prev_field = NULL;

		if (zend_hash_num_elements(&struct_type->record.fields) > 0) {
			ZEND_HASH_REVERSE_FOREACH_PTR(&struct_type->record.fields, prev_field) {
				break;
			} ZEND_HASH_FOREACH_END();
		}
		if (prev_field && prev_field->bits) {
			field->offset = prev_field->offset;
			field->first_bit = prev_field->first_bit + prev_field->bits;
			field->bits = bits->u64;
		} else {
			field->offset = struct_type->size;
			field->first_bit = 0;
			field->bits = bits->u64;
		}
		if (struct_type->attr & ZEND_FFI_ATTR_PACKED) {
			struct_type->size = field->offset + ((field->first_bit + field->bits) + 7) / 8;
		} else {
			struct_type->size = field->offset + (((field->first_bit + field->bits) + 31) / 32) * 4;
		}
	}
	field->type = field_dcl->type;
	field->is_const = (field_dcl->attr & ZEND_FFI_ATTR_CONST) != 0;
	field->is_nested = 0;
	field_dcl->type = field_type; /* reset "owned" flag */

	if (name) {
		if (!zend_hash_str_add_ptr(&struct_type->record.fields, name, name_len, field)) {
			zend_ffi_type_dtor(field->type);
			pefree(field, FFI_G(persistent));
			zend_ffi_parser_error("duplicate field name '%.*s' at line %d", name_len, name, FFI_G(line));
		}
	} else {
		zend_hash_next_index_insert_ptr(&struct_type->record.fields, field);
	}
}
/* }}} */

void zend_ffi_adjust_struct_size(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_type *struct_type = ZEND_FFI_TYPE(dcl->type);

	ZEND_ASSERT(struct_type->kind == ZEND_FFI_TYPE_STRUCT);
	if (dcl->align > struct_type->align) {
		struct_type->align = dcl->align;
	}
	if (!(struct_type->attr & ZEND_FFI_ATTR_PACKED)) {
		struct_type->size = ((struct_type->size + (struct_type->align - 1)) / struct_type->align) * struct_type->align;
	}
	dcl->align = 0;
}
/* }}} */

void zend_ffi_make_pointer_type(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_type *type = pemalloc(sizeof(zend_ffi_type), FFI_G(persistent));
	type->kind = ZEND_FFI_TYPE_POINTER;
	type->attr = (dcl->attr & ZEND_FFI_POINTER_ATTRS);
	if (FFI_G(persistent)) {
		type->attr |= ZEND_FFI_ATTR_PERSISTENT;
	}
	type->size = sizeof(void*);
	type->align = _Alignof(void*);
	zend_ffi_finalize_type(dcl);
	if (zend_ffi_validate_vla(ZEND_FFI_TYPE(dcl->type)) != SUCCESS) {
		zend_ffi_cleanup_dcl(dcl);
		LONGJMP(FFI_G(bailout), FAILURE);
	}
	type->pointer.type = dcl->type;
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
	dcl->flags &= ~ZEND_FFI_DCL_TYPE_QUALIFIERS;
	dcl->attr &= ~ZEND_FFI_POINTER_ATTRS;
	dcl->align = 0;
}
/* }}} */

static int zend_ffi_validate_array_element_type(zend_ffi_type *type) /* {{{ */
{
	if (type->kind == ZEND_FFI_TYPE_FUNC) {
		zend_ffi_throw_parser_error("array of functions is not allowed at line %d", FFI_G(line));
		return FAILURE;
	} else if (type->kind == ZEND_FFI_TYPE_ARRAY && (type->attr & ZEND_FFI_ATTR_INCOMPLETE_ARRAY)) {
		zend_ffi_throw_parser_error("only the leftmost array can be undimensioned at line %d", FFI_G(line));
		return FAILURE;
	}
	return zend_ffi_validate_type(type, 1);
}
/* }}} */

void zend_ffi_make_array_type(zend_ffi_dcl *dcl, zend_ffi_val *len) /* {{{ */
{
	int length = 0;
	zend_ffi_type *element_type;
	zend_ffi_type *type;

	zend_ffi_finalize_type(dcl);
	element_type = ZEND_FFI_TYPE(dcl->type);

	if (len->kind == ZEND_FFI_VAL_EMPTY) {
		length = 0;
	} else if (len->kind == ZEND_FFI_VAL_UINT32 || len->kind == ZEND_FFI_VAL_UINT64) {
		length = len->u64;
	} else if (len->kind == ZEND_FFI_VAL_INT32 || len->kind == ZEND_FFI_VAL_INT64) {
		length = len->i64;
	} else if (len->kind == ZEND_FFI_VAL_CHAR) {
		length = len->ch;
	} else {
		zend_ffi_cleanup_dcl(dcl);
		zend_ffi_parser_error("unsupported array index type at line %d", FFI_G(line));
		return;
	}
	if (length < 0) {
		zend_ffi_cleanup_dcl(dcl);
		zend_ffi_parser_error("negative array index at line %d", FFI_G(line));
		return;
	}

	if (zend_ffi_validate_array_element_type(element_type) != SUCCESS) {
		zend_ffi_cleanup_dcl(dcl);
		LONGJMP(FFI_G(bailout), FAILURE);
	}

	type = pemalloc(sizeof(zend_ffi_type), FFI_G(persistent));
	type->kind = ZEND_FFI_TYPE_ARRAY;
	type->attr = (dcl->attr & ZEND_FFI_ARRAY_ATTRS);
	if (FFI_G(persistent)) {
		type->attr |= ZEND_FFI_ATTR_PERSISTENT;
	}
	type->size = length * element_type->size;
	type->align = element_type->align;
	type->array.type = dcl->type;
	type->array.length = length;
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
	dcl->flags &= ~ZEND_FFI_DCL_TYPE_QUALIFIERS;
	dcl->attr &= ~ZEND_FFI_ARRAY_ATTRS;
	dcl->align = 0;
}
/* }}} */

static int zend_ffi_validate_func_ret_type(zend_ffi_type *type) /* {{{ */
{
	if (type->kind == ZEND_FFI_TYPE_FUNC) {
		zend_ffi_throw_parser_error("function returning function is not allowed at line %d", FFI_G(line));
		return FAILURE;
	 } else if (type->kind == ZEND_FFI_TYPE_ARRAY) {
		zend_ffi_throw_parser_error("function returning array is not allowed at line %d", FFI_G(line));
		return FAILURE;
	}
	return zend_ffi_validate_incomplete_type(type, 0);
}
/* }}} */

void zend_ffi_make_func_type(zend_ffi_dcl *dcl, HashTable *args) /* {{{ */
{
	zend_ffi_type *type;
	zend_ffi_type *ret_type;

	zend_ffi_finalize_type(dcl);
	ret_type = ZEND_FFI_TYPE(dcl->type);

	if (args) {
		int no_args = 0;
		zend_ffi_type *arg_type;

		ZEND_HASH_FOREACH_PTR(args, arg_type) {
			arg_type = ZEND_FFI_TYPE(arg_type);
			if (arg_type->kind == ZEND_FFI_TYPE_VOID) {
				if (zend_hash_num_elements(args) != 1) {
					zend_ffi_cleanup_dcl(dcl);
					zend_hash_destroy(args);
					pefree(args, FFI_G(persistent));
					zend_ffi_parser_error("'void' type is not allowed at line %d", FFI_G(line));
					return;
				} else {
					no_args = 1;
				}
			}
		} ZEND_HASH_FOREACH_END();
		if (no_args) {
			zend_hash_destroy(args);
			pefree(args, FFI_G(persistent));
			args = NULL;
		}
	}

	if (zend_ffi_validate_func_ret_type(ret_type) != SUCCESS) {
		zend_ffi_cleanup_dcl(dcl);
		if (args) {
			zend_hash_destroy(args);
			pefree(args, FFI_G(persistent));
		}
		LONGJMP(FFI_G(bailout), FAILURE);
	}

	type = pemalloc(sizeof(zend_ffi_type), FFI_G(persistent));
	type->kind = ZEND_FFI_TYPE_FUNC;
	type->attr = (dcl->attr & ZEND_FFI_FUNC_ATTRS);
	if (FFI_G(persistent)) {
		type->attr |= ZEND_FFI_ATTR_PERSISTENT;
	}
	type->size = sizeof(void*);
	type->align = 1;
	type->func.ret_type = dcl->type;
	switch (dcl->abi) {
		case ZEND_FFI_ABI_DEFAULT:
		case ZEND_FFI_ABI_CDECL:
			type->func.abi = FFI_DEFAULT_ABI;
			break;
		case ZEND_FFI_ABI_FASTCALL:
			type->func.abi = FFI_FASTCALL;
			break;
		case ZEND_FFI_ABI_THISCALL:
			type->func.abi = FFI_THISCALL;
			break;
		case ZEND_FFI_ABI_STDCALL:
			type->func.abi = FFI_STDCALL;
			break;
#if 0
		case ZEND_FFI_ABI_PASCAL:
			type->func.abi = FFI_PASCAL;
			break;
#endif
#if 0
		case ZEND_FFI_ABI_REGISTER:
			type->func.abi = FFI_REGISTER;
			break;
#endif
#ifdef X86_WIN32
		case ZEND_FFI_ABI_MS:
			type->func.abi = FFI_MS_CDECL;
			break;
#endif
		case ZEND_FFI_ABI_SYSV:
			type->func.abi = FFI_SYSV;
			break;
		default:
			type->func.abi = FFI_DEFAULT_ABI;
			zend_ffi_parser_error("unsupported calling convention line %d", FFI_G(line));
			break;
	}
	type->func.args = args;
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
	dcl->attr &= ~ZEND_FFI_FUNC_ATTRS;
	dcl->align = 0;
	dcl->abi = 0;
}
/* }}} */

void zend_ffi_add_arg(HashTable **args, const char *name, size_t name_len, zend_ffi_dcl *arg_dcl) /* {{{ */
{
	zend_ffi_type *type;

	if (!*args) {
		*args = pemalloc(sizeof(HashTable), FFI_G(persistent));
		zend_hash_init(*args, 0, NULL, zend_ffi_type_hash_dtor, FFI_G(persistent));
	}
	zend_ffi_finalize_type(arg_dcl);
	type = ZEND_FFI_TYPE(arg_dcl->type);
	if (type->kind == ZEND_FFI_TYPE_ARRAY) {
		if (ZEND_FFI_TYPE_IS_OWNED(arg_dcl->type)) {
			type->kind = ZEND_FFI_TYPE_POINTER;
			type->size = sizeof(void*);
		} else {
			zend_ffi_type *new_type = pemalloc(sizeof(zend_ffi_type), FFI_G(persistent));
			new_type->kind = ZEND_FFI_TYPE_POINTER;
			new_type->attr = (type->attr & ZEND_FFI_POINTER_ATTRS);
			if (FFI_G(persistent)) {
				new_type->attr |= ZEND_FFI_ATTR_PERSISTENT;
			}
			new_type->size = sizeof(void*);
			new_type->align = _Alignof(void*);
			new_type->pointer.type = ZEND_FFI_TYPE(type->array.type);
			arg_dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(new_type);
		}
	} else if (type->kind == ZEND_FFI_TYPE_FUNC) {
		zend_ffi_type *new_type = pemalloc(sizeof(zend_ffi_type), FFI_G(persistent));
		new_type->kind = ZEND_FFI_TYPE_POINTER;
		new_type->attr = 0;
		if (FFI_G(persistent)) {
			type->attr |= ZEND_FFI_ATTR_PERSISTENT;
		}
		new_type->size = sizeof(void*);
		new_type->align = _Alignof(void*);
		new_type->pointer.type = arg_dcl->type;
		arg_dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(new_type);
	}
	if (zend_ffi_validate_incomplete_type(type, 1) != SUCCESS) {
		zend_ffi_cleanup_dcl(arg_dcl);
		zend_hash_destroy(*args);
		pefree(*args, FFI_G(persistent));
		*args = NULL;
		LONGJMP(FFI_G(bailout), FAILURE);
	}
	zend_hash_next_index_insert_ptr(*args, (void*)arg_dcl->type);
}
/* }}} */

void zend_ffi_declare(const char *name, size_t name_len, zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_symbol *sym;

	if (!FFI_G(symbols)) {
		FFI_G(symbols) = pemalloc(sizeof(HashTable), FFI_G(persistent));
		zend_hash_init(FFI_G(symbols), 0, NULL, FFI_G(persistent) ? zend_ffi_symbol_hash_persistent_dtor : zend_ffi_symbol_hash_dtor, FFI_G(persistent));
	}
	sym = zend_hash_str_find_ptr(FFI_G(symbols), name, name_len);
	if (sym) {
		zend_ffi_parser_error("redeclaration of '%.*s' at line %d", name_len, name, FFI_G(line));
	} else {
		zend_ffi_finalize_type(dcl);
		if ((dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) == ZEND_FFI_DCL_TYPEDEF) {
			if (zend_ffi_validate_vla(ZEND_FFI_TYPE(dcl->type)) != SUCCESS) {
				zend_ffi_cleanup_dcl(dcl);
				LONGJMP(FFI_G(bailout), FAILURE);
			}
			if (dcl->align && dcl->align > ZEND_FFI_TYPE(dcl->type)->align) {
				if (ZEND_FFI_TYPE_IS_OWNED(dcl->type)) {
					ZEND_FFI_TYPE(dcl->type)->align = dcl->align;
				} else {
					zend_ffi_type *type = pemalloc(sizeof(zend_ffi_type), FFI_G(persistent));

					memcpy(type, ZEND_FFI_TYPE(dcl->type), sizeof(zend_ffi_type));
					if (FFI_G(persistent)) {
						type->attr |= ZEND_FFI_ATTR_PERSISTENT;
					}
					type->align = dcl->align;
					dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
				}
			}
			sym = pemalloc(sizeof(zend_ffi_symbol), FFI_G(persistent));
			sym->kind = ZEND_FFI_SYM_TYPE;
			sym->type = dcl->type;
			sym->is_const = (dcl->attr & ZEND_FFI_ATTR_CONST) != 0;
			dcl->type = ZEND_FFI_TYPE(dcl->type); /* reset "owned" flag */
			zend_hash_str_add_new_ptr(FFI_G(symbols), name, name_len, sym);
		} else {
			zend_ffi_type *type;

			type = ZEND_FFI_TYPE(dcl->type);
			if (zend_ffi_validate_type(type, 0) != SUCCESS) {
				zend_ffi_cleanup_dcl(dcl);
				LONGJMP(FFI_G(bailout), FAILURE);
			}
			if ((dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) == 0 ||
			    (dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) == ZEND_FFI_DCL_EXTERN) {
				sym = pemalloc(sizeof(zend_ffi_symbol), FFI_G(persistent));
				sym->kind = (type->kind == ZEND_FFI_TYPE_FUNC) ? ZEND_FFI_SYM_FUNC : ZEND_FFI_SYM_VAR;
				sym->type = dcl->type;
				sym->is_const = (dcl->attr & ZEND_FFI_ATTR_CONST) != 0;
				dcl->type = type; /* reset "owned" flag */
				zend_hash_str_add_new_ptr(FFI_G(symbols), name, name_len, sym);
			} else {
				/* useless declarartion */
				zend_ffi_type_dtor(dcl->type);
			}
		}
	}
}
/* }}} */

void zend_ffi_declare_tag(const char *name, size_t name_len, zend_ffi_dcl *dcl, zend_bool incomplete) /* {{{ */
{
	zend_ffi_tag *tag;

	if (!FFI_G(tags)) {
		FFI_G(tags) = pemalloc(sizeof(HashTable), FFI_G(persistent));
		zend_hash_init(FFI_G(tags), 0, NULL, FFI_G(persistent) ? zend_ffi_tag_hash_persistent_dtor : zend_ffi_tag_hash_dtor, FFI_G(persistent));
	}
	tag = zend_hash_str_find_ptr(FFI_G(tags), name, name_len);
	if (tag) {
		zend_ffi_type *type = ZEND_FFI_TYPE(tag->type);

		if (dcl->flags & ZEND_FFI_DCL_STRUCT) {
			if (tag->kind != ZEND_FFI_TAG_STRUCT) {
				zend_ffi_parser_error("'%.*s' defined as wrong kind of tag at line %d", name_len, name, FFI_G(line));
				return;
			} else if (!incomplete && !(type->attr & ZEND_FFI_ATTR_INCOMPLETE_TAG)) {
				zend_ffi_parser_error("redefinition of 'struct %.*s' at line %d", name_len, name, FFI_G(line));
				return;
			}
		} else if (dcl->flags & ZEND_FFI_DCL_UNION) {
			if (tag->kind != ZEND_FFI_TAG_UNION) {
				zend_ffi_parser_error("'%.*s' defined as wrong kind of tag at line %d", name_len, name, FFI_G(line));
				return;
			} else if (!incomplete && !(type->attr & ZEND_FFI_ATTR_INCOMPLETE_TAG)) {
				zend_ffi_parser_error("redefinition of 'union %.*s' at line %d", name_len, name, FFI_G(line));
				return;
			}
		} else if (dcl->flags & ZEND_FFI_DCL_ENUM) {
			if (tag->kind != ZEND_FFI_TAG_ENUM) {
				zend_ffi_parser_error("'%.*s' defined as wrong kind of tag at line %d", name_len, name, FFI_G(line));
				return;
			} else if (!incomplete && !(type->attr & ZEND_FFI_ATTR_INCOMPLETE_TAG)) {
				zend_ffi_parser_error("redefinition of 'enum %.*s' at line %d", name_len, name, FFI_G(line));
				return;
			}
		} else {
			ZEND_ASSERT(0);
			return;
		}
		dcl->type = type;
		if (!incomplete) {
			type->attr &= ~ZEND_FFI_ATTR_INCOMPLETE_TAG;
		}
	} else {
		zend_ffi_tag *tag = pemalloc(sizeof(zend_ffi_tag), FFI_G(persistent));

		if (dcl->flags & ZEND_FFI_DCL_STRUCT) {
			tag->kind = ZEND_FFI_TAG_STRUCT;
			zend_ffi_make_struct_type(dcl);
		} else if (dcl->flags & ZEND_FFI_DCL_UNION) {
			tag->kind = ZEND_FFI_TAG_UNION;
			zend_ffi_make_struct_type(dcl);
		} else if (dcl->flags & ZEND_FFI_DCL_ENUM) {
			tag->kind = ZEND_FFI_TAG_ENUM;
			zend_ffi_make_enum_type(dcl);
		} else {
			ZEND_ASSERT(0);
		}
		tag->type = ZEND_FFI_TYPE_MAKE_OWNED(dcl->type);
		dcl->type = ZEND_FFI_TYPE(dcl->type);
		if (incomplete) {
			dcl->type->attr |= ZEND_FFI_ATTR_INCOMPLETE_TAG;
		}
		zend_hash_str_add_new_ptr(FFI_G(tags), name, name_len, tag);
	}
}
/* }}} */

void zend_ffi_set_abi(zend_ffi_dcl *dcl, uint16_t abi) /* {{{ */
{
	if (dcl->abi != ZEND_FFI_ABI_DEFAULT) {
		zend_ffi_parser_error("multiple calling convention specifiers at line %d", FFI_G(line));
	} else {
		dcl->abi = abi;
	}
}
/* }}} */

void zend_ffi_add_attribute(zend_ffi_dcl *dcl, const char *name, size_t name_len) /* {{{ */
{
	if (name_len == sizeof("cdecl")-1 && memcmp(name, "cdecl", sizeof("cdecl")-1) == 0) {
		zend_ffi_set_abi(dcl, ZEND_FFI_ABI_CDECL);
	} else if (name_len == sizeof("fastcall")-1 && memcmp(name, "fastcall", sizeof("fastcall")-1) == 0) {
		zend_ffi_set_abi(dcl, ZEND_FFI_ABI_FASTCALL);
	} else if (name_len == sizeof("thiscall")-1 && memcmp(name, "thiscall", sizeof("thiscall")-1) == 0) {
		zend_ffi_set_abi(dcl, ZEND_FFI_ABI_THISCALL);
	} else if (name_len == sizeof("stdcall")-1 && memcmp(name, "stdcall", sizeof("stdcall")-1) == 0) {
		zend_ffi_set_abi(dcl, ZEND_FFI_ABI_STDCALL);
	} else if (name_len == sizeof("ms_abi")-1 && memcmp(name, "ms_abi", sizeof("ms_abi")-1) == 0) {
		zend_ffi_set_abi(dcl, ZEND_FFI_ABI_MS);
	} else if (name_len == sizeof("sysv_abi")-1 && memcmp(name, "sysv_abi", sizeof("sysv_abi")-1) == 0) {
		zend_ffi_set_abi(dcl, ZEND_FFI_ABI_SYSV);
	} else if (name_len == sizeof("aligned")-1 && memcmp(name, "aligned", sizeof("aligned")-1) == 0) {
		dcl->align = __BIGGEST_ALIGNMENT__;
	} else if (name_len == sizeof("packed")-1 && memcmp(name, "packed", sizeof("packed")-1) == 0) {
		dcl->attr |= ZEND_FFI_ATTR_PACKED;
	} else if (name_len == sizeof("ms_struct")-1 && memcmp(name, "ms_struct", sizeof("ms_struct")-1) == 0) {
		dcl->attr |= ZEND_FFI_ATTR_MS_STRUCT;
	} else if (name_len == sizeof("gcc_struct")-1 && memcmp(name, "gcc_struct", sizeof("gcc_struct")-1) == 0) {
		dcl->attr |= ZEND_FFI_ATTR_GCC_STRUCT;
	} else if (name_len == sizeof("const")-1 && memcmp(name, "const", sizeof("const")-1) == 0) {
		/* ignore */
	} else if (name_len == sizeof("malloc")-1 && memcmp(name, "malloc", sizeof("malloc")-1) == 0) {
		/* ignore */
	} else if (name_len == sizeof("deprecated")-1 && memcmp(name, "deprecated", sizeof("deprecated")-1) == 0) {
		/* ignore */
	} else {
		zend_ffi_parser_error("unsupported attribute '%.*s' at line %d", name_len, name, FFI_G(line));
	}
}
/* }}} */

void zend_ffi_add_attribute_value(zend_ffi_dcl *dcl, const char *name, size_t name_len, int n, zend_ffi_val *val) /* {{{ */
{
	if (n == 0 && name_len == sizeof("regparam")-1 && memcmp(name, "regparam", sizeof("regparam")-1) == 0) {
		if ((val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_INT64 || val->kind == ZEND_FFI_VAL_UINT64) && val->i64 == 3) {
			zend_ffi_set_abi(dcl, ZEND_FFI_ABI_REGISTER);
		} else {
			zend_ffi_parser_error("incorrect 'regparam' value at line %d", FFI_G(line));
		}
	} else if (n == 0 && name_len == sizeof("aligned")-1 && memcmp(name, "aligned", sizeof("aligned")-1) == 0) {
		if ((val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_INT64 || val->kind == ZEND_FFI_VAL_UINT64)
		 && val->i64 > 0 && val->i64 <= 0x80000000 && (val->i64 & (val->i64 - 1)) == 0) {
			dcl->align = val->i64;
		} else {
			zend_ffi_parser_error("incorrect 'alignemnt' value at line %d", FFI_G(line));
		}
	} else if (name_len == sizeof("format")-1 && memcmp(name, "format", sizeof("format")-1) == 0) {
		/* ignore */
	} else if (name_len == sizeof("deprecated")-1 && memcmp(name, "deprecated", sizeof("deprecated")-1) == 0) {
		/* ignore */
	} else {
		zend_ffi_parser_error("unsupported attribute '%.*s' at line %d", name_len, name, FFI_G(line));
	}
}
/* }}} */

static int zend_ffi_nested_type(zend_ffi_type *type, zend_ffi_type *nested_type) /* {{{ */
{
	nested_type = ZEND_FFI_TYPE(nested_type);
	switch (nested_type->kind) {
		case ZEND_FFI_TYPE_POINTER:
			/* "char" is used as a terminator of nested declaration */
			if (nested_type->pointer.type == &zend_ffi_type_char) {
				nested_type->pointer.type = type;
				return zend_ffi_validate_vla(ZEND_FFI_TYPE(type));
			} else {
				return zend_ffi_nested_type(type, nested_type->pointer.type);
			}
			break;
		case ZEND_FFI_TYPE_ARRAY:
			/* "char" is used as a terminator of nested declaration */
			if (nested_type->array.type == &zend_ffi_type_char) {
				nested_type->array.type = type;
				if (zend_ffi_validate_array_element_type(ZEND_FFI_TYPE(type)) != SUCCESS) {
					return FAILURE;
				}
			} else {
				if (zend_ffi_nested_type(type, nested_type->array.type) != SUCCESS) {
					return FAILURE;
				}
			}
			nested_type->size = nested_type->array.length * ZEND_FFI_TYPE(nested_type->array.type)->size;
			nested_type->align = ZEND_FFI_TYPE(nested_type->array.type)->align;
			return SUCCESS;
			break;
		case ZEND_FFI_TYPE_FUNC:
			/* "char" is used as a terminator of nested declaration */
			if (nested_type->func.ret_type == &zend_ffi_type_char) {
				nested_type->func.ret_type = type;
				return zend_ffi_validate_func_ret_type(ZEND_FFI_TYPE(type));
			} else {
				return zend_ffi_nested_type(type, nested_type->func.ret_type);
			}
			break;
		default:
			ZEND_ASSERT(0);
	}
}
/* }}} */

void zend_ffi_nested_declaration(zend_ffi_dcl *dcl, zend_ffi_dcl *nested_dcl) /* {{{ */
{
	/* "char" is used as a terminator of nested declaration */
	zend_ffi_finalize_type(dcl);
	if (!nested_dcl->type || nested_dcl->type == &zend_ffi_type_char) {
		nested_dcl->type = dcl->type;
	} else {
		if (zend_ffi_nested_type(dcl->type, nested_dcl->type) != SUCCESS) {
			zend_ffi_cleanup_dcl(nested_dcl);
			LONGJMP(FFI_G(bailout), FAILURE);
		}
	}
	dcl->type = nested_dcl->type;
}
/* }}} */

void zend_ffi_align_as_type(zend_ffi_dcl *dcl, zend_ffi_dcl *align_dcl) /* {{{ */
{
	zend_ffi_finalize_type(align_dcl);
	dcl->align = MAX(align_dcl->align, ZEND_FFI_TYPE(align_dcl->type)->align);
}
/* }}} */

void zend_ffi_align_as_val(zend_ffi_dcl *dcl, zend_ffi_val *align_val) /* {{{ */
{
	switch (align_val->kind) {
		case ZEND_FFI_VAL_INT32:
		case ZEND_FFI_VAL_UINT32:
			dcl->align = zend_ffi_type_uint32.align;
			break;
		case ZEND_FFI_VAL_INT64:
		case ZEND_FFI_VAL_UINT64:
			dcl->align = zend_ffi_type_uint64.align;
			break;
		case ZEND_FFI_VAL_FLOAT:
			dcl->align = zend_ffi_type_float.align;
			break;
		case ZEND_FFI_VAL_DOUBLE:
			dcl->align = zend_ffi_type_double.align;
			break;
		case ZEND_FFI_VAL_LONG_DOUBLE:
			dcl->align = zend_ffi_type_long_double.align;
			break;
		case ZEND_FFI_VAL_CHAR:
		case ZEND_FFI_VAL_STRING:
			dcl->align = zend_ffi_type_char.align;
			break;
		default:
			break;
	}
}
/* }}} */

#define zend_ffi_expr_bool(val) do { \
	if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) { \
		val->kind = ZEND_FFI_VAL_INT32; \
		val->i64 = !!val->u64; \
	} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) { \
		val->kind = ZEND_FFI_VAL_INT32; \
		val->i64 = !!val->i64; \
	} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
		val->kind = ZEND_FFI_VAL_INT32; \
		val->i64 = !!val->d; \
	} else if (val->kind == ZEND_FFI_VAL_CHAR) { \
		val->kind = ZEND_FFI_VAL_INT32; \
		val->i64 = !!val->ch; \
	} else { \
		val->kind = ZEND_FFI_VAL_ERROR; \
	} \
} while (0)

#define zend_ffi_expr_math(val, op2, OP) do { \
	if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = MAX(val->kind, op2->kind); \
			val->u64 = val->u64 OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32) { \
			val->u64 = val->u64 OP op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT64) { \
			val->u64 = val->u64 OP op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = op2->kind; \
			val->d = (zend_ffi_double)val->u64 OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->u64 = val->u64 OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32) { \
			val->i64 = val->i64 OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->i64 = val->i64 OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 || op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = MAX(val->kind, op2->kind); \
			val->i64 = val->i64 OP op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = op2->kind; \
			val->d = (zend_ffi_double)val->i64 OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->i64 = val->i64 OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->d = val->d OP (zend_ffi_double)op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 ||op2->kind == ZEND_FFI_VAL_INT64) { \
			val->d = val->d OP (zend_ffi_double)op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = MAX(val->kind, op2->kind); \
			val->d = val->d OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->d = val->d OP (zend_ffi_double)op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_CHAR) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = op2->kind; \
			val->u64 = val->ch OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 || op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = ZEND_FFI_VAL_INT64; \
			val->i64 = val->ch OP op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = op2->kind; \
			val->d = (zend_ffi_double)val->ch OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->ch = val->ch OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else { \
		val->kind = ZEND_FFI_VAL_ERROR; \
	} \
} while (0)

#define zend_ffi_expr_int_math(val, op2, OP) do { \
	if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = MAX(val->kind, op2->kind); \
			val->u64 = val->u64 OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32) { \
			val->u64 = val->u64 OP op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT64) { \
			val->u64 = val->u64 OP op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->u64 = val->u64 OP (uint64_t)op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->u64 = val->u64 OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32) { \
			val->i64 = val->i64 OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->i64 = val->i64 OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 || op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = MAX(val->kind, op2->kind); \
			val->i64 = val->i64 OP op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->u64 = val->u64 OP (int64_t)op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->i64 = val->i64 OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = op2->kind; \
			val->u64 = (uint64_t)val->d OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 || op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = op2->kind; \
			val->i64 = (int64_t)val->d OP op2->i64; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_CHAR) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = op2->kind; \
			val->u64 = (uint64_t)val->ch OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 || op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = op2->kind; \
			val->i64 = (int64_t)val->ch OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->ch = val->ch OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else { \
		val->kind = ZEND_FFI_VAL_ERROR; \
	} \
} while (0)

#define zend_ffi_expr_cmp(val, op2, OP) do { \
	if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->u64 OP op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 || op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->u64 OP op2->u64; /*signed/unsigned */ \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = (zend_ffi_double)val->u64 OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->u64 OP op2->d; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->i64 OP op2->i64; /* signed/unsigned */ \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 || op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->i64 OP op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = (zend_ffi_double)val->i64 OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->i64 OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->d OP (zend_ffi_double)op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 ||op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->d OP (zend_ffi_double)op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->d OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->d OP (zend_ffi_double)op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_CHAR) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->ch OP op2->i64; /* signed/unsigned */ \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 || op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->ch OP op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = (zend_ffi_double)val->ch OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->ch OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else { \
		val->kind = ZEND_FFI_VAL_ERROR; \
	} \
} while (0)

void zend_ffi_expr_conditional(zend_ffi_val *val, zend_ffi_val *op2, zend_ffi_val *op3) /* {{{ */
{
	zend_ffi_expr_bool(val);
	if (val->kind == ZEND_FFI_VAL_INT32) {
		if (val->i64) {
			*val = *op2;
		} else {
			*val = *op3;
		}
	}
}
/* }}} */

void zend_ffi_expr_bool_or(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_bool(val);
	zend_ffi_expr_bool(op2);
	if (val->kind == ZEND_FFI_VAL_INT32 && op2->kind == ZEND_FFI_VAL_INT32) {
		val->i64 = val->i64 || op2->i64;
	} else {
		val->kind = ZEND_FFI_VAL_ERROR;
	}
}
/* }}} */

void zend_ffi_expr_bool_and(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_bool(val);
	zend_ffi_expr_bool(op2);
	if (val->kind == ZEND_FFI_VAL_INT32 && op2->kind == ZEND_FFI_VAL_INT32) {
		val->i64 = val->i64 && op2->i64;
	} else {
		val->kind = ZEND_FFI_VAL_ERROR;
	}
}
/* }}} */

void zend_ffi_expr_bw_or(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_int_math(val, op2, |);
}
/* }}} */

void zend_ffi_expr_bw_xor(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_int_math(val, op2, ^);
}
/* }}} */

void zend_ffi_expr_bw_and(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_int_math(val, op2, &);
}
/* }}} */

void zend_ffi_expr_is_equal(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_cmp(val, op2, ==);
}
/* }}} */

void zend_ffi_expr_is_not_equal(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_cmp(val, op2, !=);
}
/* }}} */

void zend_ffi_expr_is_less(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_cmp(val, op2, <);
}
/* }}} */

void zend_ffi_expr_is_greater(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_cmp(val, op2, >);
}
/* }}} */

void zend_ffi_expr_is_less_or_equal(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_cmp(val, op2, <=);
}
/* }}} */

void zend_ffi_expr_is_greater_or_equal(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_cmp(val, op2, >=);
}
/* }}} */

void zend_ffi_expr_shift_left(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_int_math(val, op2, <<);
}
/* }}} */

void zend_ffi_expr_shift_right(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_int_math(val, op2, >>);
}
/* }}} */

void zend_ffi_expr_add(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_math(val, op2, +);
}
/* }}} */

void zend_ffi_expr_sub(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_math(val, op2, -);
}
/* }}} */

void zend_ffi_expr_mul(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_math(val, op2, *);
}
/* }}} */

void zend_ffi_expr_div(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_math(val, op2, /);
}
/* }}} */

void zend_ffi_expr_mod(zend_ffi_val *val, zend_ffi_val *op2) /* {{{ */
{
	zend_ffi_expr_int_math(val, op2, %); // ???
}
/* }}} */

void zend_ffi_expr_cast(zend_ffi_val *val, zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_finalize_type(dcl);
	switch (dcl->type->kind) {
		case ZEND_FFI_TYPE_FLOAT:
			if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) {
				val->kind = ZEND_FFI_VAL_FLOAT;
				val->d = val->u64;
			} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
				val->kind = ZEND_FFI_VAL_FLOAT;
				val->d = val->i64;
			} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
				val->kind = ZEND_FFI_VAL_FLOAT;
			} else if (val->kind == ZEND_FFI_VAL_CHAR) {
				val->kind = ZEND_FFI_VAL_FLOAT;
				val->d = val->ch;
			} else {
				val->kind = ZEND_FFI_VAL_ERROR;
			}
			break;
		case ZEND_FFI_TYPE_DOUBLE:
			if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) {
				val->kind = ZEND_FFI_VAL_DOUBLE;
				val->d = val->u64;
			} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
				val->kind = ZEND_FFI_VAL_DOUBLE;
				val->d = val->i64;
			} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
				val->kind = ZEND_FFI_VAL_DOUBLE;
			} else if (val->kind == ZEND_FFI_VAL_CHAR) {
				val->kind = ZEND_FFI_VAL_DOUBLE;
				val->d = val->ch;
			} else {
				val->kind = ZEND_FFI_VAL_ERROR;
			}
			break;
#ifdef HAVE_LONG_DOUBLE
		case ZEND_FFI_TYPE_LONGDOUBLE:
			if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) {
				val->kind = ZEND_FFI_VAL_LONG_DOUBLE;
				val->d = val->u64;
			} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
				val->kind = ZEND_FFI_VAL_LONG_DOUBLE;
				val->d = val->i64;
			} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
				val->kind = ZEND_FFI_VAL_LONG_DOUBLE;
			} else if (val->kind == ZEND_FFI_VAL_CHAR) {
				val->kind = ZEND_FFI_VAL_LONG_DOUBLE;
				val->d = val->ch;
			} else {
				val->kind = ZEND_FFI_VAL_ERROR;
			}
			break;
#endif
		case ZEND_FFI_TYPE_UINT8:
		case ZEND_FFI_TYPE_UINT16:
		case ZEND_FFI_TYPE_UINT32:
		case ZEND_FFI_TYPE_BOOL:
			if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64 || val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
				val->kind = ZEND_FFI_VAL_UINT32;
			} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
				val->kind = ZEND_FFI_VAL_UINT32;
				val->u64 = val->d;
			} else if (val->kind == ZEND_FFI_VAL_CHAR) {
				val->kind = ZEND_FFI_VAL_UINT32;
				val->u64 = val->ch;
			} else {
				val->kind = ZEND_FFI_VAL_ERROR;
			}
			break;
		case ZEND_FFI_TYPE_SINT8:
		case ZEND_FFI_TYPE_SINT16:
		case ZEND_FFI_TYPE_SINT32:
			if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64 || val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
				val->kind = ZEND_FFI_VAL_INT32;
			} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
				val->kind = ZEND_FFI_VAL_INT32;
				val->i64 = val->d;
			} else if (val->kind == ZEND_FFI_VAL_CHAR) {
				val->kind = ZEND_FFI_VAL_INT32;
				val->i64 = val->ch;
			} else {
				val->kind = ZEND_FFI_VAL_ERROR;
			}
			break;
		case ZEND_FFI_TYPE_UINT64:
			if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64 || val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
				val->kind = ZEND_FFI_VAL_UINT64;
			} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
				val->kind = ZEND_FFI_VAL_UINT64;
				val->u64 = val->d;
			} else if (val->kind == ZEND_FFI_VAL_CHAR) {
				val->kind = ZEND_FFI_VAL_UINT64;
				val->u64 = val->ch;
			} else {
				val->kind = ZEND_FFI_VAL_ERROR;
			}
			break;
		case ZEND_FFI_TYPE_SINT64:
			if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) {
				val->kind = ZEND_FFI_VAL_CHAR;
				val->ch = val->u64;
			} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
				val->kind = ZEND_FFI_VAL_CHAR;
				val->ch = val->i64;
			} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
				val->kind = ZEND_FFI_VAL_CHAR;
				val->ch = val->d;
			} else if (val->kind == ZEND_FFI_VAL_CHAR) {
			} else {
				val->kind = ZEND_FFI_VAL_ERROR;
			}
			break;
		case ZEND_FFI_TYPE_CHAR:
			if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64 || val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
				val->kind = ZEND_FFI_VAL_UINT32;
			} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
				val->kind = ZEND_FFI_VAL_UINT32;
				val->u64 = val->d;
			} else if (val->kind == ZEND_FFI_VAL_CHAR) {
				val->kind = ZEND_FFI_VAL_UINT32;
				val->u64 = val->ch;
			} else {
				val->kind = ZEND_FFI_VAL_ERROR;
			}
			break;
		default:
			val->kind = ZEND_FFI_VAL_ERROR;
			break;
	}
}
/* }}} */

void zend_ffi_expr_plus(zend_ffi_val *val) /* {{{ */
{
	if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) {
	} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
	} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
	} else if (val->kind == ZEND_FFI_VAL_CHAR) {
	} else {
		val->kind = ZEND_FFI_VAL_ERROR;
	}
}
/* }}} */

void zend_ffi_expr_neg(zend_ffi_val *val) /* {{{ */
{
	if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) {
		val->u64 = -val->u64;
	} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
		val->i64 = -val->i64;
	} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
		val->d = -val->d;
	} else if (val->kind == ZEND_FFI_VAL_CHAR) {
		val->ch = -val->ch;
	} else {
		val->kind = ZEND_FFI_VAL_ERROR;
	}
}
/* }}} */

void zend_ffi_expr_bw_not(zend_ffi_val *val) /* {{{ */
{
	if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) {
		val->u64 = ~val->u64;
	} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
		val->i64 = ~val->i64;
	} else if (val->kind == ZEND_FFI_VAL_CHAR) {
		val->ch = ~val->ch;
	} else {
		val->kind = ZEND_FFI_VAL_ERROR;
	}
}
/* }}} */

void zend_ffi_expr_bool_not(zend_ffi_val *val) /* {{{ */
{
	zend_ffi_expr_bool(val);
	if (val->kind == ZEND_FFI_VAL_INT32) {
		val->i64 = !val->i64;
	}
}
/* }}} */

void zend_ffi_expr_sizeof_val(zend_ffi_val *val) /* {{{ */
{
	if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_INT32) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_uint32.size;
	} else if (val->kind == ZEND_FFI_VAL_UINT64 || val->kind == ZEND_FFI_VAL_INT64) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_uint64.size;
	} else if (val->kind == ZEND_FFI_VAL_FLOAT) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_float.size;
	} else if (val->kind == ZEND_FFI_VAL_DOUBLE) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_double.size;
	} else if (val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_long_double.size;
	} else if (val->kind == ZEND_FFI_VAL_CHAR) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_char.size;
	} else if (val->kind == ZEND_FFI_VAL_STRING) {
		if (memchr(val->str, '\\', val->len)) {
			// TODO: support for escape sequences ???
			val->kind = ZEND_FFI_VAL_ERROR;
		} else {
			val->kind = ZEND_FFI_VAL_UINT32;
			val->u64 = val->len + 1;
		}
	} else {
		val->kind = ZEND_FFI_VAL_ERROR;
	}
}
/* }}} */

void zend_ffi_expr_sizeof_type(zend_ffi_val *val, zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_finalize_type(dcl);
	val->kind = (dcl->type->size > 0xffffffff) ? ZEND_FFI_VAL_UINT64 : ZEND_FFI_VAL_UINT32;
	val->u64 = dcl->type->size;
}
/* }}} */

void zend_ffi_expr_alignof_val(zend_ffi_val *val) /* {{{ */
{
	if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_INT32) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_uint32.align;
	} else if (val->kind == ZEND_FFI_VAL_UINT64 || val->kind == ZEND_FFI_VAL_INT64) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_uint64.align;
	} else if (val->kind == ZEND_FFI_VAL_FLOAT) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_float.align;
	} else if (val->kind == ZEND_FFI_VAL_DOUBLE) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_double.align;
	} else if (val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_long_double.align;
	} else if (val->kind == ZEND_FFI_VAL_CHAR) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = zend_ffi_type_char.size;
	} else if (val->kind == ZEND_FFI_VAL_STRING) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = _Alignof(char*);
	} else {
		val->kind = ZEND_FFI_VAL_ERROR;
	}
}
/* }}} */

void zend_ffi_expr_alignof_type(zend_ffi_val *val, zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_finalize_type(dcl);
	val->kind = ZEND_FFI_VAL_UINT32;
	val->u64 = dcl->type->align;
}
/* }}} */

void zend_ffi_val_number(zend_ffi_val *val, int base, const char *str, size_t str_len) /* {{{ */
{
	int u = 0;
	int l = 0;

	if (str[str_len-1] == 'u' || str[str_len-1] == 'U') {
		u = 1;
		if (str[str_len-2] == 'l' || str[str_len-2] == 'L') {
			l = 1;
			if (str[str_len-3] == 'l' || str[str_len-3] == 'L') {
				l = 2;
			}
		}
	} else if (str[str_len-1] == 'l' || str[str_len-1] == 'L') {
		l = 1;
		if (str[str_len-2] == 'l' || str[str_len-2] == 'L') {
			l = 2;
			if (str[str_len-3] == 'u' || str[str_len-3] == 'U') {
				u = 1;
			}
		} else if (str[str_len-2] == 'u' || str[str_len-2] == 'U') {
			u = 1;
		}
	}
	if (u) {
		val->u64 = strtoull(str, NULL, base);
		if (l == 0) {
			val->kind = ZEND_FFI_VAL_UINT32;
		} else if (l == 1) {
			val->kind = (sizeof(long) == 4) ? ZEND_FFI_VAL_UINT32 : ZEND_FFI_VAL_UINT64;
		} else if (l == 2) {
			val->kind = ZEND_FFI_VAL_UINT64;
		}
	} else {
		val->i64 = strtoll(str, NULL, base);
		if (l == 0) {
			val->kind = ZEND_FFI_VAL_INT32;
		} else if (l == 1) {
			val->kind = (sizeof(long) == 4) ? ZEND_FFI_VAL_INT32 : ZEND_FFI_VAL_INT64;
		} else if (l == 2) {
			val->kind = ZEND_FFI_VAL_INT64;
		}
	}
}
/* }}} */

void zend_ffi_val_float_number(zend_ffi_val *val, const char *str, size_t str_len) /* {{{ */
{
	val->d = strtold(str, NULL);
	if (str[str_len-1] == 'f' || str[str_len-1] == 'F') {
		val->kind = ZEND_FFI_VAL_FLOAT;
	} else if (str[str_len-1] == 'l' || str[str_len-1] == 'L') {
		val->kind = ZEND_FFI_VAL_LONG_DOUBLE;
	} else {
		val->kind = ZEND_FFI_VAL_DOUBLE;
	}
}
/* }}} */

void zend_ffi_val_string(zend_ffi_val *val, const char *str, size_t str_len) /* {{{ */
{
	if (str[0] != '\"') {
		val->kind = ZEND_FFI_VAL_ERROR;
	} else {
		val->kind = ZEND_FFI_VAL_STRING;
		val->str = str + 1;
		val->len = str_len - 2;
	}
}
/* }}} */

void zend_ffi_val_character(zend_ffi_val *val, const char *str, size_t str_len) /* {{{ */
{
	int n;

	if (str[0] != '\'') {
		val->kind = ZEND_FFI_VAL_ERROR;
	} else {
		val->kind = ZEND_FFI_VAL_CHAR;
		if (str_len == 3) {
			val->ch = str[1];
		} else if (str[1] == '\\') {
			if (str[2] == 'a') {
			} else if (str[2] == 'b' && str_len == 4) {
				val->ch = '\b';
			} else if (str[2] == 'f' && str_len == 4) {
				val->ch = '\f';
			} else if (str[2] == 'n' && str_len == 4) {
				val->ch = '\n';
			} else if (str[2] == 'r' && str_len == 4) {
				val->ch = '\r';
			} else if (str[2] == 't' && str_len == 4) {
				val->ch = '\t';
			} else if (str[2] == 'v' && str_len == 4) {
				val->ch = '\v';
			} else if (str[2] >= '0' || str[2] <= '7') {
				n = str[2] - '0';
				if (str[3] >= '0' || str[3] <= '7') {
					n = n * 8 + (str[3] - '0');
					if ((str[4] >= '0' || str[4] <= '7') && str_len == 6) {
						n = n * 8 + (str[4] - '0');
					} else if (str_len != 5) {
						val->kind = ZEND_FFI_VAL_ERROR;
					}
				} else if (str_len != 4) {
					val->kind = ZEND_FFI_VAL_ERROR;
				}
				if (n <= 0xff) {
					val->ch = n;
				} else {
					val->kind = ZEND_FFI_VAL_ERROR;
				}
			} else if (str[2] == 'x') {
				if (str[3] >= '0' || str[3] <= '7') {
					n = str[3] - '0';
				} else if (str[3] >= 'A' || str[3] <= 'F') {
					n = str[3] - 'A';
				} else if (str[3] >= 'a' || str[3] <= 'f') {
					n = str[3] - 'a';
				} else {
					val->kind = ZEND_FFI_VAL_ERROR;
				}
				if ((str[4] >= '0' || str[4] <= '7') && str_len == 6) {
					n = n * 16 + (str[4] - '0');
				} else if ((str[4] >= 'A' || str[4] <= 'F') && str_len == 6) {
					n = n * 16 + (str[4] - 'A');
				} else if ((str[4] >= 'a' || str[4] <= 'f') && str_len == 6) {
					n = n * 16 + (str[4] - 'a');
				} else if (str_len != 5) {
					val->kind = ZEND_FFI_VAL_ERROR;
				}
				val->ch = n;
			} else if (str_len == 4) {
				val->ch = str[2];
			} else {
				val->kind = ZEND_FFI_VAL_ERROR;
			}
		} else {
			val->kind = ZEND_FFI_VAL_ERROR;
		}
	}
}
/* }}} */

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * End:
 */
