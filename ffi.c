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
#include "zend_exceptions.h"

#include <ffi.h>

ZEND_DECLARE_MODULE_GLOBALS(ffi)

typedef enum _zend_ffi_tag_kind {
	ZEND_FFI_TAG_ENUM,
	ZEND_FFI_TAG_STRUCT,
	ZEND_FFI_TAG_UNION
} zend_ffi_tag_kind;

typedef struct _zend_ffi_tag {
	zend_ffi_tag_kind      kind;
	zend_ffi_type         *type;
} zend_ffi_tag;

typedef enum _zend_ffi_type_kind {
	ZEND_FFI_TYPE_VOID,
	ZEND_FFI_TYPE_FLOAT,
	ZEND_FFI_TYPE_DOUBLE,
	ZEND_FFI_TYPE_LONGDOUBLE,
	ZEND_FFI_TYPE_UINT8,
	ZEND_FFI_TYPE_SINT8,
	ZEND_FFI_TYPE_UINT16,
	ZEND_FFI_TYPE_SINT16,
	ZEND_FFI_TYPE_UINT32,
	ZEND_FFI_TYPE_SINT32,
	ZEND_FFI_TYPE_UINT64,
	ZEND_FFI_TYPE_SINT64,
	ZEND_FFI_TYPE_ENUM,
	ZEND_FFI_TYPE_CHAR,
	ZEND_FFI_TYPE_POINTER,
	ZEND_FFI_TYPE_FUNC,
	ZEND_FFI_TYPE_ARRAY,
	ZEND_FFI_TYPE_STRUCT,
} zend_ffi_type_kind;

struct _zend_ffi_type {
	zend_ffi_type_kind     kind;
	uint32_t               attr;
	size_t                 size;
	union {
		struct {
			int64_t last;
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
			zend_bool      variadic;
			HashTable      args;
			ffi_abi        abi;
		} func;
	};
};

typedef struct _zend_ffi_field {
	size_t                 offset;
	zend_ffi_type         *type;
} zend_ffi_field;

typedef enum _zend_ffi_symbol_kind {
	ZEND_FFI_SYM_TYPE,
	ZEND_FFI_SYM_CONST,
	ZEND_FFI_SYM_VAR,
	ZEND_FFI_SYM_FUNC
} zend_ffi_symbol_kind;

typedef struct _zend_ffi_symbol {
	zend_ffi_symbol_kind kind;
	zend_ffi_type *type;
	union {
		void *addr;
		int64_t value;
	};
} zend_ffi_symbol;

typedef struct _zend_ffi {
	zend_object            std;
	DL_HANDLE              lib;
	HashTable             *symbols;
	HashTable             *tags;
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
	zend_bool              owned_ptr;
} zend_ffi_cdata;

static zend_class_entry *zend_ffi_exception_ce;
static zend_class_entry *zend_ffi_parser_exception_ce;
static zend_class_entry *zend_ffi_ce;
static zend_class_entry *zend_ffi_cdata_ce;

static zend_object_handlers zend_ffi_handlers;
static zend_object_handlers zend_ffi_cdata_handlers;

static zend_internal_function zend_ffi_new_fn;

/* forward declarations */
static void zend_ffi_finalize_type(zend_ffi_dcl *dcl);

static zend_object *zend_ffi_cdata_new(zend_class_entry *class_type) /* {{{ */
{
	zend_ffi_cdata *cdata;

	cdata = emalloc(sizeof(zend_ffi_cdata));
	memset(cdata, 0, sizeof(zend_ffi_cdata));

	zend_object_std_init(&cdata->std, class_type);
	cdata->std.handlers = &zend_ffi_cdata_handlers;

	cdata->type = NULL;
	cdata->ptr = NULL;
	cdata->owned_ptr = 0;

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
		} else {
			break;
		}
	}
	return 0;
}
/* }}} */

static int zend_ffi_cdata_to_zval(zend_ffi_cdata *cdata, void *ptr, zend_ffi_type *type, int read_type, zval *rv) /* {{{ */
{
	if (read_type == BP_VAR_R && type->kind < ZEND_FFI_TYPE_ARRAY) {
	    switch (type->kind) {
			case ZEND_FFI_TYPE_FLOAT:
				ZVAL_DOUBLE(rv, *(float*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_DOUBLE:
				ZVAL_DOUBLE(rv, *(double*)ptr);
				return SUCCESS;
#ifndef PHP_WIN32
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
			case ZEND_FFI_TYPE_CHAR:
				ZVAL_INTERNED_STR(rv, ZSTR_CHAR(*(unsigned char*)ptr));
				return SUCCESS;
			case ZEND_FFI_TYPE_ENUM:
				// TODO: enum type/size???
				ZVAL_LONG(rv, *(uint32_t*)ptr);
				return SUCCESS;
			case ZEND_FFI_TYPE_POINTER:
				if (*(void**)ptr == NULL) {
					ZVAL_NULL(rv);
					return SUCCESS;
				} else if (ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_CHAR) {
					ZVAL_STRING(rv, *(char**)ptr);
					return SUCCESS;
				}
				break;
			case ZEND_FFI_TYPE_FUNC:
				if (*(void**)ptr == NULL) {
					ZVAL_NULL(rv);
					return SUCCESS;
				}
				break;
			default:
				break;
		}
	}

	if (!cdata) {
		cdata = (zend_ffi_cdata*)zend_ffi_cdata_new(zend_ffi_cdata_ce);
		cdata->type = type;
		cdata->ptr = ptr;
	} else {
		GC_ADDREF(&cdata->std);
	}

	ZVAL_OBJ(rv, &cdata->std);
	return SUCCESS;
}
/* }}} */

static int zend_ffi_zval_to_cdata(void *ptr, zend_ffi_type *type, zval *value) /* {{{ */
{
	zend_long lval;
	double dval;
	zend_string *tmp_str;
	zend_string *str;

    switch (type->kind) {
		case ZEND_FFI_TYPE_FLOAT:
			dval = zval_get_long(value);
			*(float*)ptr = dval;
			break;
		case ZEND_FFI_TYPE_DOUBLE:
			dval = zval_get_long(value);
			*(double*)ptr = dval;
			break;
#ifndef PHP_WIN32
		case ZEND_FFI_TYPE_LONGDOUBLE:
			dval = zval_get_long(value);
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
			// TODO: enum type/size???
			lval = zval_get_long(value);
			*(int32_t*)ptr = lval;
			break;
		case ZEND_FFI_TYPE_FUNC:
			if (Z_TYPE_P(value) == IS_NULL) {
				*(void**)ptr = NULL;
				break;
			}
			zend_throw_error(zend_ffi_exception_ce, "Attempt to perform assign of incompatible C type");
			return FAILURE;
		case ZEND_FFI_TYPE_POINTER:
			if (Z_TYPE_P(value) == IS_NULL) {
				*(void**)ptr = NULL;
				break;
			}
			if (Z_TYPE_P(value) == IS_OBJECT && Z_OBJCE_P(value) == zend_ffi_cdata_ce) {
				zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(value);
				void *cdata_ptr;

				if (cdata->owned_ptr) {
					// TODO: assignment of owned pointers is not safe, because it may be destroied by PHP (leak in tests/006.phpt) ???
					type = ZEND_FFI_TYPE(type->pointer.type);
					cdata_ptr = cdata->ptr;
				} else {
					cdata_ptr = *(void**)cdata->ptr;;
				}
				if (zend_ffi_is_compatible_type(type, ZEND_FFI_TYPE(cdata->type))) {
					*(void**)ptr = cdata_ptr;
					return SUCCESS;
				}
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

static ZEND_COLD zend_function *zend_ffi_cdata_get_constructor(zend_object *object) /* {{{ */
{
	zend_throw_error(zend_ffi_exception_ce, "Instantiation of 'CData' is not allowed");
	return NULL;
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

	ptr = (void*)(((char*)cdata->ptr) + field->offset);
	if (zend_ffi_cdata_to_zval(NULL, ptr, ZEND_FFI_TYPE(field->type), read_type, rv) != SUCCESS) {
		return &EG(uninitialized_zval);
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

	ptr = (void*)(((char*)cdata->ptr) + field->offset);
	zend_ffi_zval_to_cdata(ptr, ZEND_FFI_TYPE(field->type), value);
}
/* }}} */

static zval *zend_ffi_cdata_read_dim(zval *object, zval *offset, int read_type, zval *rv) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	zend_long       dim = zval_get_long(offset);
	void           *ptr;

	if (type->kind == ZEND_FFI_TYPE_ARRAY) {
		if (dim < 0 || (type->array.length && dim >= type->array.length)) {
			zend_throw_error(zend_ffi_exception_ce, "C array index out of bounds");
			return &EG(uninitialized_zval);
		}

		type = ZEND_FFI_TYPE(type->array.type);
		ptr = (void*)(((char*)cdata->ptr) + type->size * dim);
	} else if (type->kind == ZEND_FFI_TYPE_POINTER) {
		type = ZEND_FFI_TYPE(type->pointer.type);
		ptr = (void*)((*(char**)cdata->ptr) + type->size * dim);
	} else {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to read element of non C array");
		return &EG(uninitialized_zval);
	}

	if (zend_ffi_cdata_to_zval(NULL, ptr, type, read_type, rv) != SUCCESS) {
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

	if (type->kind == ZEND_FFI_TYPE_ARRAY) {
		if (dim < 0 || (type->array.length && dim >= type->array.length)) {
			zend_throw_error(zend_ffi_exception_ce, "C array index out of bounds");
			return;
		}

		type = ZEND_FFI_TYPE(type->array.type);
		ptr = (void*)(((char*)cdata->ptr) + type->size * dim);
	} else if (type->kind == ZEND_FFI_TYPE_POINTER) {
		type = ZEND_FFI_TYPE(type->pointer.type);
		ptr = (void*)((*(char**)cdata->ptr) + type->size * dim);
	} else {
		zend_throw_error(zend_ffi_exception_ce, "Attempt to assign element of non C array");
		return;
	}


	zend_ffi_zval_to_cdata(ptr, type, value);
}
/* }}} */

// TODO: ???
//static int zend_ffi_cdata_compare_objects(zval *o1, zval *o2) /* {{{ */
//{
//	// TODO: ???
//	return 0;
//}
///* }}} */
//
//static int zend_ffi_cdata_cast_object(zval *readobj, zval *writeobj, int type) /* {{{ */
//{
//	//zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(readobj);
//
//	// TODO: ???
//	switch (type) {
//		case IS_LONG:
//			break;
//		case IS_DOUBLE:
//			break;
//		case IS_STRING:
//			break;
//		default:
//			break;
//	}
//	return FAILURE;
//}
///* }}} */

PHP_FFI_API HashTable *zend_ffi_cdata_get_debug_info(zval *object, int *is_temp) /* {{{ */
{
	zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(object);
	zend_ffi_type  *type = ZEND_FFI_TYPE(cdata->type);
	void           *ptr = cdata->ptr;
	HashTable      *ht = NULL;
	zend_string    *key;
	zend_ffi_field *f;
	zend_long       n;
	zval            tmp;

    switch (type->kind) {
		case ZEND_FFI_TYPE_CHAR:
		case ZEND_FFI_TYPE_ENUM:
		case ZEND_FFI_TYPE_FLOAT:
		case ZEND_FFI_TYPE_DOUBLE:
		case ZEND_FFI_TYPE_LONGDOUBLE:
		case ZEND_FFI_TYPE_UINT8:
		case ZEND_FFI_TYPE_SINT8:
		case ZEND_FFI_TYPE_UINT16:
		case ZEND_FFI_TYPE_SINT16:
		case ZEND_FFI_TYPE_UINT32:
		case ZEND_FFI_TYPE_SINT32:
		case ZEND_FFI_TYPE_UINT64:
		case ZEND_FFI_TYPE_SINT64:
			if (zend_ffi_cdata_to_zval(cdata, ptr, type, BP_VAR_R, &tmp) == SUCCESS) {
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
				zend_hash_str_add(ht, "cptr", sizeof("cptr")-1, &tmp);
				*is_temp = 1;
				return ht;
			} else if (zend_ffi_cdata_to_zval(cdata, *(void**)ptr, ZEND_FFI_TYPE(type->pointer.type), BP_VAR_R, &tmp) == SUCCESS) {
				ht = zend_new_array(1);
				zend_hash_str_add(ht, "cptr", sizeof("cptr")-1, &tmp);
				*is_temp = 1;
				return ht;
			}
			break;
		case ZEND_FFI_TYPE_STRUCT:
			ht = zend_new_array(zend_hash_num_elements(&type->record.fields));
			ZEND_HASH_FOREACH_STR_KEY_PTR(&type->record.fields, key, f) {
				void *f_ptr = (void*)(((char*)ptr) + f->offset);
				if (zend_ffi_cdata_to_zval(NULL, f_ptr, ZEND_FFI_TYPE(f->type), BP_VAR_R, &tmp) == SUCCESS) {
					zend_hash_add(ht, key, &tmp);
				}
			} ZEND_HASH_FOREACH_END();
			*is_temp = 1;
			return ht;
		case ZEND_FFI_TYPE_ARRAY:
			ht = zend_new_array(type->array.length);
			for (n = 0; n < type->array.length; n++) {
				if (zend_ffi_cdata_to_zval(NULL, ptr, ZEND_FFI_TYPE(type->array.type), BP_VAR_R, &tmp) == SUCCESS) {
					zend_hash_index_add(ht, n, &tmp);
				}
				ptr = (void*)(((char*)ptr) + ZEND_FFI_TYPE(type->array.type)->size);
			}
			*is_temp = 1;
			return ht;
		default:
			break;
	}
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
			zend_hash_destroy(&type->func.args);
			zend_ffi_type_dtor(type->func.ret_type);
			break;
		default:
			break;
	}
	efree(type);
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

static void zend_ffi_symbol_hash_dtor(zval *zv) /* {{{ */
{
	zend_ffi_symbol *sym = Z_PTR_P(zv);
	zend_ffi_type_dtor(sym->type);
	efree(sym);
}
/* }}} */

static void zend_ffi_tag_hash_dtor(zval *zv) /* {{{ */
{
	zend_ffi_tag *tag = Z_PTR_P(zv);
	zend_ffi_type_dtor(tag->type);
	efree(tag);
}
/* }}} */

static void zend_ffi_cdata_dtor(zend_ffi_cdata *cdata) /* {{{ */
{
	zend_ffi_type_dtor(cdata->type);
	if (cdata->owned_ptr) {
		efree(cdata->ptr);
	}
}
/* }}} */

static void zend_ffi_free_obj(zend_object *object) /* {{{ */
{
	zend_ffi *ffi = (zend_ffi*)object;

	zend_object_std_dtor(&ffi->std);

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


static zval *zend_ffi_read_var(zval *object, zval *member, int read_type, void **cache_slot, zval *rv) /* {{{ */
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
		zend_throw_error(zend_ffi_exception_ce, "Attempt to read undefined C variable '%s'", ZSTR_VAL(var_name));
		zend_tmp_string_release(tmp_var_name);
		return &EG(uninitialized_zval);
	}

	zend_tmp_string_release(tmp_var_name);

	if (zend_ffi_cdata_to_zval(NULL, sym->addr, ZEND_FFI_TYPE(sym->type), read_type, rv) != SUCCESS) {
		return &EG(uninitialized_zval);
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

	zend_ffi_zval_to_cdata(sym->addr, ZEND_FFI_TYPE(sym->type), value);
}
/* }}} */

static int zend_ffi_pass_arg(zval *arg, zend_ffi_type *type, ffi_type **pass_type, void *pass_val) /* {{{ */
{
	zend_long lval;
	double dval;
	zend_string *str, *tmp_str;

	ZVAL_DEREF(arg);
	switch (type->kind) {
		case ZEND_FFI_TYPE_FLOAT:
			dval = zval_get_double(arg);
			*pass_type = &ffi_type_float;
			*(float*)pass_val = (float)dval;
			break;
		case ZEND_FFI_TYPE_DOUBLE:
			dval = zval_get_double(arg);
			*pass_type = &ffi_type_double;
			*(double*)pass_val = dval;
			break;
#ifndef PHP_WIN32
		case ZEND_FFI_TYPE_LONGDOUBLE:
			dval = zval_get_double(arg);
			*pass_type = &ffi_type_double;
			*(long double*)pass_val = (long double)dval;
			break;
#endif
		case ZEND_FFI_TYPE_UINT8:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_uint8;
			*(uint8_t*)pass_val = (uint8_t)lval;
			break;
		case ZEND_FFI_TYPE_SINT8:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_sint8;
			*(int8_t*)pass_val = (int8_t)lval;
			break;
		case ZEND_FFI_TYPE_UINT16:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_uint16;
			*(uint16_t*)pass_val = (uint16_t)lval;
			break;
		case ZEND_FFI_TYPE_SINT16:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_sint16;
			*(int16_t*)pass_val = (int16_t)lval;
			break;
		case ZEND_FFI_TYPE_UINT32:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_uint32;
			*(uint32_t*)pass_val = (uint32_t)lval;
			break;
		case ZEND_FFI_TYPE_SINT32:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_sint32;
			*(int32_t*)pass_val = (int32_t)lval;
			break;
		case ZEND_FFI_TYPE_UINT64:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_uint64;
			*(uint64_t*)pass_val = (uint64_t)lval;
			break;
		case ZEND_FFI_TYPE_SINT64:
			lval = zval_get_long(arg);
			*pass_type = &ffi_type_sint64;
			*(int64_t*)pass_val = (int64_t)lval;
			break;
		case ZEND_FFI_TYPE_POINTER:
			*pass_type = &ffi_type_pointer;
			if (Z_TYPE_P(arg) == IS_NULL) {
				*(void**)pass_val = NULL;
				return SUCCESS;
			} else if (Z_TYPE_P(arg) == IS_STRING 
			        && ((ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_CHAR)
			         || (ZEND_FFI_TYPE(type->pointer.type)->kind == ZEND_FFI_TYPE_VOID))) {
				*(void**)pass_val = Z_STRVAL_P(arg);
				return SUCCESS;
			} else if (Z_TYPE_P(arg) == IS_OBJECT && Z_OBJCE_P(arg) == zend_ffi_cdata_ce) {
				zend_ffi_cdata *cdata = (zend_ffi_cdata*)Z_OBJ_P(arg);
				void *cdata_ptr;

				if (cdata->owned_ptr) {
					type = ZEND_FFI_TYPE(type->pointer.type);
					cdata_ptr = cdata->ptr;
				} else {
					cdata_ptr = *(void**)cdata->ptr;;
				}
				if (zend_ffi_is_compatible_type(type, ZEND_FFI_TYPE(cdata->type))) {
					*(void**)pass_val = cdata_ptr;
					return SUCCESS;
				}
			}
			zend_throw_error(zend_ffi_exception_ce, "FFI passing pointer is not implemented");
			return FAILURE;
		case ZEND_FFI_TYPE_FUNC:
			*pass_type = &ffi_type_pointer;
			if (Z_TYPE_P(arg) == IS_NULL) {
				*(void**)pass_val = NULL;
			} else {
				zend_throw_error(zend_ffi_exception_ce, "FFI passing function pointer is not implemented");
				return FAILURE;
			}
			break;
		case ZEND_FFI_TYPE_CHAR:
			str = zval_get_tmp_string(arg, &tmp_str);
			*pass_type = &ffi_type_sint8;
			*(char*)pass_val = ZSTR_VAL(str)[0];
			if (ZSTR_LEN(str) != 1) {
				zend_throw_error(zend_ffi_exception_ce, "Attempt to pass incompatible C type");
			}
			zend_tmp_string_release(tmp_str);
			break;
		case ZEND_FFI_TYPE_ENUM:
			lval = zval_get_long(arg);
			// TODO: enum type/size?
			*pass_type = &ffi_type_sint32;
			*(int32_t*)pass_val = (int32_t)lval;
			break;
		case ZEND_FFI_TYPE_STRUCT:
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
			*pass_type = &ffi_type_sint32;
			*(int32_t*)pass_val = 0;
			break;
		case IS_TRUE:
			*pass_type = &ffi_type_sint32;
			*(int32_t*)pass_val = 0;
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

static ffi_type *zend_ffi_ret_type(zend_ffi_type *type) /* {{{ */
{
	switch (type->kind) {
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
		case ZEND_FFI_TYPE_FUNC:
			return &ffi_type_pointer;
		case ZEND_FFI_TYPE_VOID:
			return &ffi_type_void;
		case ZEND_FFI_TYPE_CHAR:
			return &ffi_type_sint8;
		case ZEND_FFI_TYPE_ENUM:
			// TODO: enum type/size?
			return &ffi_type_sint32;
		case ZEND_FFI_TYPE_STRUCT:
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

static ZEND_FUNCTION(ffi_trampoline) /* {{{ */
{
	zend_ffi_symbol *sym = EX(func)->internal_function.reserved[0];
	zend_ffi_type *type = ZEND_FFI_TYPE(sym->type);
	ffi_cif cif;
	ffi_type *ret_type = NULL;
	ffi_type **arg_types = NULL;
	void **arg_values = NULL;
	uint32_t n, arg_count;
	ffi_arg ret;
	zend_ffi_type *arg_type;
	ALLOCA_FLAG(arg_types_use_heap)
	ALLOCA_FLAG(arg_values_use_heap)

	ZEND_ASSERT(type->kind == ZEND_FFI_TYPE_FUNC);
	arg_count = zend_hash_num_elements(&type->func.args);
	if (type->func.variadic) {
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
			ZEND_HASH_FOREACH_PTR(&type->func.args, arg_type) {
				arg_type = ZEND_FFI_TYPE(arg_type);
				arg_values[n] = ((char*)arg_values) + (sizeof(void*) * EX_NUM_ARGS()) + (FFI_SIZEOF_ARG * n);
				if (zend_ffi_pass_arg(EX_VAR_NUM(n), arg_type, &arg_types[n], arg_values[n]) != SUCCESS) {
					free_alloca(arg_types, arg_types_use_heap);
					free_alloca(arg_values, arg_values_use_heap);
					return;
				}
				n++;
			} ZEND_HASH_FOREACH_END();
			for (; n < EX_NUM_ARGS(); n++) {
				arg_values[n] = ((char*)arg_values) + (sizeof(void*) * EX_NUM_ARGS()) + (FFI_SIZEOF_ARG * n);
				if (zend_ffi_pass_var_arg(EX_VAR_NUM(n), &arg_types[n], arg_values[n]) != SUCCESS) {
					free_alloca(arg_types, arg_types_use_heap);
					free_alloca(arg_values, arg_values_use_heap);
					return;
				}
			}
		}
		ret_type = zend_ffi_ret_type(ZEND_FFI_TYPE(type->func.ret_type));
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
			ZEND_HASH_FOREACH_PTR(&type->func.args, arg_type) {
				arg_type = ZEND_FFI_TYPE(arg_type);
				arg_values[n] = ((char*)arg_values) + (sizeof(void*) * EX_NUM_ARGS()) + (FFI_SIZEOF_ARG * n);
				if (zend_ffi_pass_arg(EX_VAR_NUM(n), arg_type, &arg_types[n], arg_values[n]) != SUCCESS) {
					free_alloca(arg_types, arg_types_use_heap);
					free_alloca(arg_values, arg_values_use_heap);
					return;
				}
				n++;
			} ZEND_HASH_FOREACH_END();
		}
		ret_type = zend_ffi_ret_type(ZEND_FFI_TYPE(type->func.ret_type));
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

	ffi_call(&cif, sym->addr, &ret, arg_values);

	free_alloca(arg_types, arg_types_use_heap);
	free_alloca(arg_values, arg_values_use_heap);

	zend_ffi_cdata_to_zval(NULL, (void*)&ret, ZEND_FFI_TYPE(type->func.ret_type), BP_VAR_R, return_value);

	zend_string_release(EX(func)->common.function_name);
	zend_free_trampoline(EX(func));
	EX(func) = NULL;
}
/* }}} */

static zend_function *zend_ffi_get_func(zend_object **obj, zend_string *name, const zval *key) /* {{{ */
{
	zend_ffi               *ffi = (zend_ffi*)*obj;
	zend_ffi_symbol        *sym = NULL;
	zend_internal_function *func;
	zend_ffi_type          *type;

	if (ZSTR_LEN(name) == sizeof("new") -1
	 && (ZSTR_VAL(name)[0] == 'n' || ZSTR_VAL(name)[0] == 'N')
	 && (ZSTR_VAL(name)[1] == 'e' || ZSTR_VAL(name)[1] == 'E')
	 && (ZSTR_VAL(name)[2] == 'w' || ZSTR_VAL(name)[2] == 'W')) {
		return (zend_function*)&zend_ffi_new_fn;
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

	// TODO: setup trampoline function (arg_info) ???
	if (EXPECTED(EG(trampoline).common.function_name == NULL)) {
		func = &EG(trampoline).internal_function;
	} else {
		func = ecalloc(sizeof(zend_internal_function), 1);
	}
	func->type = ZEND_INTERNAL_FUNCTION;
	func->arg_flags[0] = 0;
	func->arg_flags[1] = 0;
	func->arg_flags[2] = 0;
	func->fn_flags = ZEND_ACC_CALL_VIA_TRAMPOLINE;
	func->function_name = zend_string_copy(name);
	func->num_args = func->required_num_args = zend_hash_num_elements(&type->func.args);
	func->handler = ZEND_FN(ffi_trampoline);

	func->reserved[0] = sym;

	return (zend_function*)func;
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
	}

	/* Parse C definitions */
	FFI_G(error) = NULL;
	FFI_G(symbols) = NULL;
	FFI_G(tags) = NULL;

	if (zend_ffi_parse_decl(code) != SUCCESS) {
		zend_throw_error(zend_ffi_parser_exception_ce, FFI_G(error));
		efree(FFI_G(error));
		FFI_G(error) = NULL;
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
/* }}} */

ZEND_METHOD(FFI, new) /* {{{ */
{
	zend_string *type_def;
	zend_ffi_dcl dcl = {0,0,NULL};
	zend_ffi_type *type;
	zend_ffi_cdata *cdata;
	void *ptr;

	ZEND_PARSE_PARAMETERS_START(1, 1)
		Z_PARAM_STR(type_def)
	ZEND_PARSE_PARAMETERS_END();

	FFI_G(error) = NULL;
	if (Z_TYPE(EX(This)) == IS_OBJECT) {
		zend_ffi *ffi = (zend_ffi*)Z_OBJ(EX(This));
		FFI_G(symbols) = ffi->symbols;
		FFI_G(tags) = ffi->tags;
	} else {
		FFI_G(symbols) = NULL;
		FFI_G(tags) = NULL;
	}


	if (zend_ffi_parse_type(type_def, &dcl) == SUCCESS) {
		zend_ffi_finalize_type(&dcl);
	}

	type = ZEND_FFI_TYPE(dcl.type);

	if (type == NULL || FFI_G(error)) {
		if (FFI_G(error)) {
			zend_throw_error(zend_ffi_parser_exception_ce, FFI_G(error));
		} else {
			zend_throw_error(zend_ffi_parser_exception_ce, "incorrect C type '%s'", ZSTR_VAL(type_def));
		}
		efree(FFI_G(error));
		FFI_G(error) = NULL;
		return;
	}

	ptr = emalloc(type->size);
	memset(ptr, 0, type->size);

	cdata = (zend_ffi_cdata*)zend_ffi_cdata_new(zend_ffi_cdata_ce);
	cdata->type = dcl.type;
	cdata->ptr = ptr;
	cdata->owned_ptr = 1;

	RETURN_OBJ(&cdata->std);
}
/* }}} */

static const zend_function_entry zend_ffi_functions[] = {
	ZEND_ME(FFI, __construct, NULL,  ZEND_ACC_PUBLIC)
	ZEND_ME(FFI, new, NULL,  ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	ZEND_FE_END
};

/* {{{ ZEND_MINIT_FUNCTION
 */
ZEND_MINIT_FUNCTION(ffi)
{
	zend_class_entry ce;

	INIT_CLASS_ENTRY(ce, "FFIException", NULL);
	zend_ffi_exception_ce = zend_register_internal_class_ex(&ce, zend_ce_error);
	zend_ffi_exception_ce->ce_flags |= ZEND_ACC_FINAL;

	INIT_CLASS_ENTRY(ce, "FFIParserException", NULL);
	zend_ffi_parser_exception_ce = zend_register_internal_class_ex(&ce, zend_ce_error);
	zend_ffi_parser_exception_ce->ce_flags |= ZEND_ACC_FINAL;

	INIT_CLASS_ENTRY(ce, "FFI", zend_ffi_functions);
	zend_ffi_ce = zend_register_internal_class(&ce);
	zend_ffi_ce->ce_flags |= ZEND_ACC_FINAL;
	zend_ffi_ce->create_object = zend_ffi_new;

	memcpy(&zend_ffi_new_fn, zend_hash_str_find_ptr(&zend_ffi_ce->function_table, "new", sizeof("new")-1), sizeof(zend_internal_function));
	zend_ffi_new_fn.fn_flags &= ~ZEND_ACC_STATIC;

	memcpy(&zend_ffi_handlers, zend_get_std_object_handlers(), sizeof(zend_object_handlers));
	zend_ffi_handlers.free_obj             = zend_ffi_free_obj;
	zend_ffi_handlers.clone_obj            = NULL;
	zend_ffi_handlers.read_property        = zend_ffi_read_var;
	zend_ffi_handlers.write_property       = zend_ffi_write_var;
	zend_ffi_handlers.read_dimension       = NULL;
	zend_ffi_handlers.write_dimension      = NULL;
	zend_ffi_handlers.get_property_ptr_ptr = NULL;
	zend_ffi_handlers.has_property         = NULL;
	zend_ffi_handlers.unset_property       = NULL;
	zend_ffi_handlers.has_dimension        = NULL;
	zend_ffi_handlers.unset_dimension      = NULL;
	zend_ffi_handlers.get_properties       = NULL;
	zend_ffi_handlers.get_method           = zend_ffi_get_func;
	zend_ffi_handlers.call_method          = NULL;
	zend_ffi_handlers.compare_objects      = NULL;
	zend_ffi_handlers.cast_object          = NULL;
	zend_ffi_handlers.get_debug_info       = NULL;
	zend_ffi_handlers.get_closure          = NULL;
	zend_ffi_handlers.get_gc               = NULL;

	INIT_CLASS_ENTRY(ce, "CData", NULL);
	zend_ffi_cdata_ce = zend_register_internal_class(&ce);
	zend_ffi_cdata_ce->ce_flags |= ZEND_ACC_FINAL;
	zend_ffi_cdata_ce->create_object = zend_ffi_cdata_new;

	memcpy(&zend_ffi_cdata_handlers, zend_get_std_object_handlers(), sizeof(zend_object_handlers));
	zend_ffi_cdata_handlers.get_constructor      = zend_ffi_cdata_get_constructor;
	zend_ffi_cdata_handlers.free_obj             = zend_ffi_cdata_free_obj;
	zend_ffi_cdata_handlers.clone_obj            = NULL;
	zend_ffi_cdata_handlers.read_property        = zend_ffi_cdata_read_field;
	zend_ffi_cdata_handlers.write_property       = zend_ffi_cdata_write_field;
	zend_ffi_cdata_handlers.read_dimension       = zend_ffi_cdata_read_dim;
	zend_ffi_cdata_handlers.write_dimension      = zend_ffi_cdata_write_dim;
	zend_ffi_cdata_handlers.get_property_ptr_ptr = NULL;
	zend_ffi_cdata_handlers.has_property         = NULL;
	zend_ffi_cdata_handlers.unset_property       = NULL;
	zend_ffi_cdata_handlers.has_dimension        = NULL;
	zend_ffi_cdata_handlers.unset_dimension      = NULL;
	zend_ffi_cdata_handlers.get_properties       = NULL;
	zend_ffi_cdata_handlers.get_method           = NULL;
	zend_ffi_cdata_handlers.call_method          = NULL;
	zend_ffi_cdata_handlers.compare_objects      = NULL; // TODO:??? zend_ffi_cdata_compare_objects;
	zend_ffi_cdata_handlers.cast_object          = NULL; // TODO:??? zend_ffi_cdata_cast_object;
	zend_ffi_cdata_handlers.get_debug_info       = zend_ffi_cdata_get_debug_info;
	zend_ffi_cdata_handlers.get_closure          = NULL;
	zend_ffi_cdata_handlers.get_gc               = NULL;

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
}
/* }}} */

#define ZEND_FFI_VERSION "0.1.0"

static const zend_ffi_type zend_ffi_type_void = {.kind=ZEND_FFI_TYPE_VOID, .size=0};
static const zend_ffi_type zend_ffi_type_char = {.kind=ZEND_FFI_TYPE_CHAR, .size=1};
static const zend_ffi_type zend_ffi_type_sint8 = {.kind=ZEND_FFI_TYPE_SINT8, .size=1};
static const zend_ffi_type zend_ffi_type_uint8 = {.kind=ZEND_FFI_TYPE_UINT8, .size=1};
static const zend_ffi_type zend_ffi_type_sint16 = {.kind=ZEND_FFI_TYPE_SINT16, .size=2};
static const zend_ffi_type zend_ffi_type_uint16 = {.kind=ZEND_FFI_TYPE_UINT16, .size=2};
static const zend_ffi_type zend_ffi_type_sint32 = {.kind=ZEND_FFI_TYPE_SINT32, .size=4};
static const zend_ffi_type zend_ffi_type_uint32 = {.kind=ZEND_FFI_TYPE_UINT32, .size=4};
static const zend_ffi_type zend_ffi_type_sint64 = {.kind=ZEND_FFI_TYPE_SINT64, .size=8};
static const zend_ffi_type zend_ffi_type_uint64 = {.kind=ZEND_FFI_TYPE_UINT64, .size=8};
static const zend_ffi_type zend_ffi_type_float = {.kind=ZEND_FFI_TYPE_FLOAT, .size=4};
static const zend_ffi_type zend_ffi_type_double = {.kind=ZEND_FFI_TYPE_DOUBLE, .size=8};
static const zend_ffi_type zend_ffi_type_long_double = {.kind=ZEND_FFI_TYPE_LONGDOUBLE, .size=10};

const struct {
	const char *name;
	const zend_ffi_type *type;
} zend_ffi_types[] = {
	{"void",        &zend_ffi_type_void},
	{"char",        &zend_ffi_type_char},
	{"bool",        &zend_ffi_type_uint8},
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
#ifndef PHP_WIN32
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
	NULL,					/* ZEND_RSHUTDOWN - Request shutdown */
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
static void zend_ffi_finalize_type(zend_ffi_dcl *dcl)
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
				if (!FFI_G(error)) {
					zend_spprintf(&FFI_G(error), 0, "unsupported type '_Complex' at line %d", FFI_G(line));
				}
				break;
			default:
				if (!FFI_G(error)) {
					zend_spprintf(&FFI_G(error), 0, "unsupported type specifier combination at line %d", FFI_G(line));
				}
				break;
		}
		dcl->flags &= ~ZEND_FFI_DCL_TYPE_SPECIFIERS;
		dcl->flags |= ZEND_FFI_DCL_TYPEDEF_NAME;
	} else {
		zend_ffi_type *type = ZEND_FFI_TYPE(dcl->type);

		if ((type->attr & ZEND_FFI_ATTR_INCOMPLETE) && !FFI_G(error)) {
			if (FFI_G(tags)) {
				zend_string *key;
				zend_ffi_tag *tag;

				ZEND_HASH_FOREACH_STR_KEY_PTR(FFI_G(tags), key, tag) {
					if (ZEND_FFI_TYPE(tag->type) == type) {
						if (type->kind == ZEND_FFI_TYPE_ENUM) {
							zend_spprintf(&FFI_G(error), 0, "incomplete 'enum %s' at line %d", ZSTR_VAL(key), FFI_G(line));
						} else if (type->attr & ZEND_FFI_ATTR_UNION) {
							zend_spprintf(&FFI_G(error), 0, "incomplete 'union %s' at line %d", ZSTR_VAL(key), FFI_G(line));
						} else {
							zend_spprintf(&FFI_G(error), 0, "incomplete 'struct %s' at line %d", ZSTR_VAL(key), FFI_G(line));
						}
						return;
					}
				} ZEND_HASH_FOREACH_END();
			}
			zend_spprintf(&FFI_G(error), 0, "incomplete type at line %d", FFI_G(line));
		}
	}
}

int zend_ffi_is_typedef_name(const char *name, size_t name_len) /* {{{ */
{
	zend_ffi_symbol *sym;
	zend_ffi_type *type;

	if (!FFI_G(error)) {
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

	}
	return 0;
}
/* }}} */

zend_ffi_type *zend_ffi_resolve_typedef(const char *name, size_t name_len) /* {{{ */
{
	zend_ffi_symbol *sym;
	zend_ffi_type *type;

	if (!FFI_G(error)) {
		if (FFI_G(symbols)) {
			sym = zend_hash_str_find_ptr(FFI_G(symbols), name, name_len);
			if (sym && sym->kind == ZEND_FFI_SYM_TYPE) {
				type = ZEND_FFI_TYPE(sym->type);
				if (type->attr & ZEND_FFI_ATTR_INCOMPLETE) {
					zend_spprintf(&FFI_G(error), 0, "incomplete C type '%.*s' at line %d", name_len, name, FFI_G(line));
					return NULL;
				}
				return type;
			}
		}
		type = zend_hash_str_find_ptr(&FFI_G(types), name, name_len);
		if (type) {
			return type;
		}
		zend_spprintf(&FFI_G(error), 0, "undefined C type '%.*s' at line %d", name_len, name, FFI_G(line));
	}
	return NULL;
}
/* }}} */

void zend_ffi_resolve_const(const char *name, size_t name_len, zend_ffi_val *val) /* {{{ */
{
	zend_ffi_symbol *sym;

	if (!FFI_G(error)) {
		if (FFI_G(symbols)) {
			sym = zend_hash_str_find_ptr(FFI_G(symbols), name, name_len);
			if (sym && sym->kind == ZEND_FFI_SYM_CONST) {
				// TODO: enum type/size???
				val->kind = ZEND_FFI_VAL_INT32;
				val->i64 = sym->value;
				return;
			}
		}
	}
	val->kind = ZEND_FFI_VAL_ERROR;
}
/* }}} */

void zend_ffi_make_enum_type(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_type *type = emalloc(sizeof(zend_ffi_type));
	type->kind = ZEND_FFI_TYPE_ENUM;
	type->attr = (dcl->attr & ZEND_FFI_ENUM_ATTRS);
	type->size = sizeof(int);
	type->enumeration.last = -1;
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
}
/* }}} */

void zend_ffi_add_enum_val(zend_ffi_dcl *enum_dcl, const char *name, size_t name_len, zend_ffi_val *val) /* {{{ */
{
	if (!FFI_G(error)) {
		zend_ffi_symbol *sym;
		int64_t value;
		zend_ffi_type *enum_type = ZEND_FFI_TYPE(enum_dcl->type);

		ZEND_ASSERT(enum_type && enum_type->kind == ZEND_FFI_TYPE_ENUM);
		if (val->kind == ZEND_FFI_VAL_EMPTY) {
			value = enum_type->enumeration.last + 1;
		} else if (val->kind == ZEND_FFI_VAL_CHAR) {
			value = val->ch;
		} else if (val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_INT64) {
			value = val->i64;
		} else if (val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_UINT64) {
			value = val->u64;
		} else {
			zend_spprintf(&FFI_G(error), 0, "enumerator value must be an integer at line %d", FFI_G(line));
			return;
		}
	
		if (!FFI_G(symbols)) {
			ALLOC_HASHTABLE(FFI_G(symbols));
			zend_hash_init(FFI_G(symbols), 0, NULL, zend_ffi_symbol_hash_dtor, 0);
		}
		sym = zend_hash_str_find_ptr(FFI_G(symbols), name, name_len);
		if (sym) {
			zend_spprintf(&FFI_G(error), 0, "redeclaration of '%.*s' at line %d", name_len, name, FFI_G(line));
		} else {
			sym = emalloc(sizeof(zend_ffi_symbol));
			sym->kind  = ZEND_FFI_SYM_CONST;
			sym->type  = NULL;
			sym->value = value;
			enum_type->enumeration.last = value;
			if (value > 0xffffffff) {
				// enum/type size???
				enum_type->size = 8;
			}
			zend_hash_str_add_new_ptr(FFI_G(symbols), name, name_len, sym);
		}
	}
}
/* }}} */

void zend_ffi_make_struct_type(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_type *type = emalloc(sizeof(zend_ffi_type));
	type->kind = ZEND_FFI_TYPE_STRUCT;
	type->attr = (dcl->attr & ZEND_FFI_STRUCT_ATTRS);
	type->size = 0;
	if (dcl->flags & ZEND_FFI_DCL_UNION) {
		type->attr |= ZEND_FFI_ATTR_UNION;
	}
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
	zend_hash_init(&type->record.fields, 0, NULL, zend_ffi_field_hash_dtor, 0);
}
/* }}} */

void zend_ffi_add_field(zend_ffi_dcl *struct_dcl, const char *name, size_t name_len, zend_ffi_dcl *field_dcl) /* {{{ */
{
	if (!FFI_G(error)) {
		zend_ffi_field *field = emalloc(sizeof(zend_ffi_field));
		zend_ffi_type *struct_type = ZEND_FFI_TYPE(struct_dcl->type);
		zend_ffi_type *field_type;

		ZEND_ASSERT(struct_type && struct_type->kind == ZEND_FFI_TYPE_STRUCT);
		zend_ffi_finalize_type(field_dcl);
		field_type = ZEND_FFI_TYPE(field_dcl->type);
		if (struct_type->attr & ZEND_FFI_ATTR_UNION) {
			field->offset = 0;
			struct_type->size = MAX(struct_type->size, field_type->size);
		} else if (struct_type->attr & ZEND_FFI_ATTR_PACKED) {
			field->offset = struct_type->size;
			struct_type->size += field_type->size;
		} else {
			// TODO: alignment ???
			field->offset = struct_type->size;
			struct_type->size += field_type->size;
		}
		field->type = field_dcl->type;
		field_dcl->type = field_type; /* reset "owned" flag */

		zend_hash_str_add_ptr(&struct_type->record.fields, name, name_len, field);
	}
}
/* }}} */

void zend_ffi_add_bit_field(zend_ffi_dcl *struct_dcl, const char *name, size_t name_len, zend_ffi_dcl *field_dcl, zend_ffi_val *bits) /* {{{ */
{
	if (!FFI_G(error)) {
		zend_ffi_type *struct_type = ZEND_FFI_TYPE(struct_dcl->type);

		ZEND_ASSERT(struct_type && struct_type->kind == ZEND_FFI_TYPE_STRUCT);
		zend_ffi_finalize_type(field_dcl);
		/* TODO: bit fields??? */
//		zend_spprintf(&FFI_G(error), 0, "bit fields are not supported at line %d", FFI_G(line));
	}
}
/* }}} */

void zend_ffi_skip_bit_field(zend_ffi_dcl *struct_dcl, zend_ffi_val *bits) /* {{{ */
{
	if (!FFI_G(error)) {
		zend_ffi_type *struct_type = ZEND_FFI_TYPE(struct_dcl->type);

		ZEND_ASSERT(struct_type && struct_type->kind == ZEND_FFI_TYPE_STRUCT);
		/* TODO: bit fields??? */
//		zend_spprintf(&FFI_G(error), 0, "bit fields are not supported at line %d", FFI_G(line));
	}
}
/* }}} */

void zend_ffi_make_pointer_type(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_type *type = emalloc(sizeof(zend_ffi_type));
	type->kind = ZEND_FFI_TYPE_POINTER;
	type->attr = (dcl->attr & ZEND_FFI_POINTER_ATTRS);
	type->size = sizeof(void*);
	zend_ffi_finalize_type(dcl);
	type->pointer.type = dcl->type;
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
}
/* }}} */

void zend_ffi_make_array_type(zend_ffi_dcl *dcl, zend_ffi_val *len) /* {{{ */
{
	int length = 0;
	zend_ffi_type *element_type;

	if (len->kind == ZEND_FFI_VAL_EMPTY) {
		length = 0;
	} else if (len->kind == ZEND_FFI_VAL_UINT32 || len->kind == ZEND_FFI_VAL_UINT64) {
		length = len->u64;
	} else if (len->kind == ZEND_FFI_VAL_INT32 || len->kind == ZEND_FFI_VAL_INT64) {
		length = len->i64;
	} else if (len->kind == ZEND_FFI_VAL_CHAR) {
		length = len->ch;
	} else {
		if (!FFI_G(error)) {
			zend_spprintf(&FFI_G(error), 0, "unsupported array index type at line %d", FFI_G(line));
		}
		length = 0;
	}
	if (length < 0) {
		if (!FFI_G(error)) {
			zend_spprintf(&FFI_G(error), 0, "negative array index at line %d", FFI_G(line));
		}
		length = 0;
	}

	if (length == 0) {
		// TODO: array with zero size???
//		if (!FFI_G(error)) {
//			zend_spprintf(&FFI_G(error), 0, "unknown array size at line %d", FFI_G(line));
//		}
		length = 0;
	}

	zend_ffi_type *type = emalloc(sizeof(zend_ffi_type));
	type->kind = ZEND_FFI_TYPE_ARRAY;
	type->attr = (dcl->attr & ZEND_FFI_ARRAY_ATTRS);
	zend_ffi_finalize_type(dcl);
	element_type = ZEND_FFI_TYPE(dcl->type);
	// TODO: alignment ???
	type->size = length * element_type->size;
	type->array.type = dcl->type;
	type->array.length = length;
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
}
/* }}} */

void zend_ffi_make_func_type(zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_type *type = emalloc(sizeof(zend_ffi_type));
	type->kind = ZEND_FFI_TYPE_FUNC;
	type->attr = (dcl->attr & ZEND_FFI_FUNC_ATTRS);
	type->size = sizeof(void*);
	zend_ffi_finalize_type(dcl);
	type->func.ret_type = dcl->type;
	type->func.variadic = 0;
	// TODO: verify ABI ???
	type->func.abi = FFI_DEFAULT_ABI; //???
	zend_hash_init(&type->func.args, 0, NULL, zend_ffi_type_hash_dtor, 0);
	dcl->type = ZEND_FFI_TYPE_MAKE_OWNED(type);
}
/* }}} */

void zend_ffi_add_arg(zend_ffi_dcl *func_dcl, const char *name, size_t name_len, zend_ffi_dcl *arg_dcl) /* {{{ */
{
	if (!FFI_G(error)) {
		zend_ffi_type *func_type = ZEND_FFI_TYPE(func_dcl->type);

		ZEND_ASSERT(func_type && func_type->kind == ZEND_FFI_TYPE_FUNC);
		zend_ffi_finalize_type(arg_dcl);
		zend_hash_next_index_insert_ptr(&func_type->func.args, (void*)arg_dcl->type);
	}
}
/* }}} */

void zend_ffi_add_variadic_arg(zend_ffi_dcl *func_dcl) /* {{{ */
{
	if (!FFI_G(error)) {
		zend_ffi_type *func_type = ZEND_FFI_TYPE(func_dcl->type);

		ZEND_ASSERT(func_type && func_type->kind == ZEND_FFI_TYPE_FUNC);
		func_type->func.variadic = 1;
	}
}
/* }}} */

void zend_ffi_declare(const char *name, size_t name_len, zend_ffi_dcl *dcl) /* {{{ */
{
	zend_ffi_symbol *sym;

	if (!FFI_G(error)) {
		if (!FFI_G(symbols)) {
			ALLOC_HASHTABLE(FFI_G(symbols));
			zend_hash_init(FFI_G(symbols), 0, NULL, zend_ffi_symbol_hash_dtor, 0);
		}
		sym = zend_hash_str_find_ptr(FFI_G(symbols), name, name_len);
		if (sym) {
			zend_spprintf(&FFI_G(error), 0, "redeclaration of '%.*s' at line %d", name_len, name, FFI_G(line));
		} else {
			if ((dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) == ZEND_FFI_DCL_TYPEDEF) {
				if (!dcl->type) {
					zend_ffi_finalize_type(dcl);
				}
				sym = emalloc(sizeof(zend_ffi_symbol));
				sym->kind = ZEND_FFI_SYM_TYPE;
				sym->type = dcl->type;
				dcl->type = ZEND_FFI_TYPE(dcl->type); /* reset "owned" flag */
				zend_hash_str_add_new_ptr(FFI_G(symbols), name, name_len, sym);
			} else if ((dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) == 0 ||
			           (dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) == ZEND_FFI_DCL_EXTERN) {
				zend_ffi_type *type;

				zend_ffi_finalize_type(dcl);
				sym = emalloc(sizeof(zend_ffi_symbol));
				type = ZEND_FFI_TYPE(dcl->type);
				sym->kind = (type->kind == ZEND_FFI_TYPE_FUNC) ? ZEND_FFI_SYM_FUNC : ZEND_FFI_SYM_VAR;
				sym->type = dcl->type;
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

	if (!FFI_G(error)) {
		if (!FFI_G(tags)) {
			ALLOC_HASHTABLE(FFI_G(tags));
			zend_hash_init(FFI_G(tags), 0, NULL, zend_ffi_tag_hash_dtor, 0);
		}
		tag = zend_hash_str_find_ptr(FFI_G(tags), name, name_len);
		if (tag) {
			zend_ffi_type *type = ZEND_FFI_TYPE(tag->type);

			if (dcl->flags & ZEND_FFI_DCL_STRUCT) {
				if (tag->kind != ZEND_FFI_TAG_STRUCT) {
					zend_spprintf(&FFI_G(error), 0, "'%.*s' defined as wrong kind of tag at line %d", name_len, name, FFI_G(line));
					return;
				} else if (!incomplete && !(type->attr & ZEND_FFI_ATTR_INCOMPLETE)) {
					zend_spprintf(&FFI_G(error), 0, "redefinition of 'struct %.*s' at line %d", name_len, name, FFI_G(line));
					return;
				}
			} else if (dcl->flags & ZEND_FFI_DCL_UNION) {
				if (tag->kind != ZEND_FFI_TAG_UNION) {
					zend_spprintf(&FFI_G(error), 0, "'%.*s' defined as wrong kind of tag at line %d", name_len, name, FFI_G(line));
					return;
				} else if (!incomplete && !(type->attr & ZEND_FFI_ATTR_INCOMPLETE)) {
					zend_spprintf(&FFI_G(error), 0, "redefinition of 'union %.*s' at line %d", name_len, name, FFI_G(line));
					return;
				}
			} else if (dcl->flags & ZEND_FFI_DCL_ENUM) {
				if (tag->kind != ZEND_FFI_TAG_ENUM) {
					zend_spprintf(&FFI_G(error), 0, "'%.*s' defined as wrong kind of tag at line %d", name_len, name, FFI_G(line));
					return;
				} else if (!incomplete && !(type->attr & ZEND_FFI_ATTR_INCOMPLETE)) {
					zend_spprintf(&FFI_G(error), 0, "redefinition of 'enum %.*s' at line %d", name_len, name, FFI_G(line));
					return;
				}
			} else {
				ZEND_ASSERT(0);
				return;
			}
			dcl->type = type;
			if (!incomplete) {
				type->attr &= ~ZEND_FFI_ATTR_INCOMPLETE;
			}
		} else {
			zend_ffi_tag *tag = emalloc(sizeof(zend_ffi_tag));

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
				dcl->type->attr |= ZEND_FFI_ATTR_INCOMPLETE;
			}
			zend_hash_str_add_new_ptr(FFI_G(tags), name, name_len, tag);
		}
	}
}
/* }}} */

void zend_ffi_add_attribute(zend_ffi_dcl *dcl, const char *name, size_t name_len) /* {{{ */
{
	if (name_len == sizeof("cdecl")-1 && memcmp(name, "cdecl", sizeof("cdecl")-1) == 0) {
		dcl->attr |= ZEND_FFI_ATTR_CDECL;
	} else if (name_len == sizeof("fastcall")-1 && memcmp(name, "fastcall", sizeof("fastcall")-1) == 0) {
		dcl->attr |= ZEND_FFI_ATTR_FASTCALL;
	} else if (name_len == sizeof("thiscall")-1 && memcmp(name, "thiscall", sizeof("thiscall")-1) == 0) {
		dcl->attr |= ZEND_FFI_ATTR_THISCALL;
	} else if (name_len == sizeof("stdcall")-1 && memcmp(name, "stdcall", sizeof("stdcall")-1) == 0) {
		dcl->attr |= ZEND_FFI_ATTR_STDCALL;
	} else if (name_len == sizeof("ms_abi")-1 && memcmp(name, "ms_abi", sizeof("ms_abi")-1) == 0) {
		dcl->attr |= ZEND_FFI_ATTR_MS_ABI;
	} else if (name_len == sizeof("sysv_abi")-1 && memcmp(name, "sysv_abi", sizeof("sysv_abi")-1) == 0) {
		dcl->attr |= ZEND_FFI_ATTR_SYSV_ABI;
	} else if (name_len == sizeof("aligned")-1 && memcmp(name, "aligned", sizeof("aligned")-1) == 0) {
		// alignment ???
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
		if (!FFI_G(error)) {
			zend_spprintf(&FFI_G(error), 0, "unsupported attribute '%.*s' at line %d", name_len, name, FFI_G(line));
		}
	}
}
/* }}} */

void zend_ffi_add_attribute_value(zend_ffi_dcl *dcl, const char *name, size_t name_len, int n, zend_ffi_val *val) /* {{{ */
{
	if (n == 0 && name_len == sizeof("regparam")-1 && memcmp(name, "regparam", sizeof("regparam")-1) == 0) {
		if ((val->kind == ZEND_FFI_VAL_INT32 || val->kind == ZEND_FFI_VAL_UINT32 || val->kind == ZEND_FFI_VAL_INT64 || val->kind == ZEND_FFI_VAL_UINT64) && val->i64 > 0 && val->i64 <= 3) {
			if (val->i64 == 1) {
				dcl->attr |= ZEND_FFI_ATTR_REGPARAM_1;
			} else if (val->i64 == 2) {
				dcl->attr |= ZEND_FFI_ATTR_REGPARAM_2;
			} else {
				dcl->attr |= ZEND_FFI_ATTR_REGPARAM_3;
			}
		} else {
			if (!FFI_G(error)) {
				zend_spprintf(&FFI_G(error), 0, "incorrect 'regparam' value at line %d", FFI_G(line));
			}
		}
	} else if (n == 0 && name_len == sizeof("aligned")-1 && memcmp(name, "aligned", sizeof("aligned")-1) == 0) {
		// alignment ???
	} else if (name_len == sizeof("format")-1 && memcmp(name, "format", sizeof("format")-1) == 0) {
		/* ignore */
	} else if (name_len == sizeof("deprecated")-1 && memcmp(name, "deprecated", sizeof("deprecated")-1) == 0) {
		/* ignore */
	} else {
		if (!FFI_G(error)) {
			zend_spprintf(&FFI_G(error), 0, "unsupported attribute '%.*s' at line %d", name_len, name, FFI_G(line));
		}
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
			val->d = (long double)val->u64 OP op2->d; \
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
			val->d = (long double)val->i64 OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->i64 = val->i64 OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->d = val->d OP (long double)op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 ||op2->kind == ZEND_FFI_VAL_INT64) { \
			val->d = val->d OP (long double)op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = MAX(val->kind, op2->kind); \
			val->d = val->d OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->d = val->d OP (long double)op2->ch; \
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
			val->d = (long double)val->ch OP op2->d; \
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
			val->i64 = (long double)val->u64 OP op2->d; \
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
			val->i64 = (long double)val->i64 OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->i64 OP op2->ch; \
		} else { \
			val->kind = ZEND_FFI_VAL_ERROR; \
		} \
	} else if (val->kind == ZEND_FFI_VAL_FLOAT || val->kind == ZEND_FFI_VAL_DOUBLE || val->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
		if (op2->kind == ZEND_FFI_VAL_UINT32 || op2->kind == ZEND_FFI_VAL_UINT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->d OP (long double)op2->u64; \
		} else if (op2->kind == ZEND_FFI_VAL_INT32 ||op2->kind == ZEND_FFI_VAL_INT64) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->d OP (long double)op2->i64; \
		} else if (op2->kind == ZEND_FFI_VAL_FLOAT || op2->kind == ZEND_FFI_VAL_DOUBLE || op2->kind == ZEND_FFI_VAL_LONG_DOUBLE) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->d OP op2->d; \
		} else if (op2->kind == ZEND_FFI_VAL_CHAR) { \
			val->kind = ZEND_FFI_VAL_INT32; \
			val->i64 = val->d OP (long double)op2->ch; \
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
			val->i64 = (long double)val->ch OP op2->d; \
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
#ifndef PHP_WIN32
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
		val->u64 = 4;
	} else if (val->kind == ZEND_FFI_VAL_UINT64 || val->kind == ZEND_FFI_VAL_INT64) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = 8;
	} else if (val->kind == ZEND_FFI_VAL_FLOAT) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = 4;
	} else if (val->kind == ZEND_FFI_VAL_DOUBLE) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = 8;
	} else if (val->kind == ZEND_FFI_VAL_LONG_DOUBLE) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = 10;
	} else if (val->kind == ZEND_FFI_VAL_CHAR) {
		val->kind = ZEND_FFI_VAL_UINT32;
		val->u64 = 1;
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

void zend_ffi_val_error(zend_ffi_val *val) /* {{{ */
{
	val->kind = ZEND_FFI_VAL_ERROR;
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
