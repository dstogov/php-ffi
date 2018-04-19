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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define yy_buf  FFI_G(buf)
#define yy_end  FFI_G(end)
#define yy_pos  FFI_G(pos)
#define yy_text FFI_G(text)
#define yy_line FFI_G(line)

#define yy_error(msg) { \
		if (!FFI_G(error)) { \
			zend_spprintf(&FFI_G(error), 0, "%s at line %d", msg, yy_line); \
		} \
	} while (0)

#define yy_error_sym(msg, sym) { \
		if (!FFI_G(error)) { \
			zend_spprintf(&FFI_G(error), 0, msg " at line %d", sym_name[sym], yy_line); \
		} \
	} while (0)

#define YYPOS cpos
#define YYEND cend

#define YY_EOF 0
#define YY__COMMA 1
#define YY__SEMICOLON 2
#define YY_TYPEDEF 3
#define YY_EXTERN 4
#define YY_STATIC 5
#define YY_AUTO 6
#define YY_REGISTER 7
#define YY_INLINE 8
#define YY__NORETURN 9
#define YY__ALIGNAS 10
#define YY__LPAREN 11
#define YY__RPAREN 12
#define YY_CONST 13
#define YY_RESTRICT 14
#define YY_VOLATILE 15
#define YY__ATOMIC 16
#define YY_VOID 17
#define YY_CHAR 18
#define YY_SHORT 19
#define YY_INT 20
#define YY_LONG 21
#define YY_FLOAT 22
#define YY_DOUBLE 23
#define YY_SIGNED 24
#define YY_UNSIGNED 25
#define YY__BOOL 26
#define YY__COMPLEX 27
#define YY_STRUCT 28
#define YY_UNION 29
#define YY__LBRACE 30
#define YY__RBRACE 31
#define YY__COLON 32
#define YY_ENUM 33
#define YY__EQUAL 34
#define YY__STAR 35
#define YY__LBRACK 36
#define YY__RBRACK 37
#define YY__POINT_POINT_POINT 38
#define YY___ATTRIBUTE__ 39
#define YY__POINT 40
#define YY__QUERY 41
#define YY__BAR_BAR 42
#define YY__AND_AND 43
#define YY__BAR 44
#define YY__UPARROW 45
#define YY__AND 46
#define YY__EQUAL_EQUAL 47
#define YY__BANG_EQUAL 48
#define YY__LESS 49
#define YY__GREATER 50
#define YY__LESS_EQUAL 51
#define YY__GREATER_EQUAL 52
#define YY__LESS_LESS 53
#define YY__GREATER_GREATER 54
#define YY__PLUS 55
#define YY__MINUS 56
#define YY__SLASH 57
#define YY__PERCENT 58
#define YY__MINUS_GREATER 59
#define YY__PLUS_PLUS 60
#define YY__MINUS_MINUS 61
#define YY__TILDE 62
#define YY__BANG 63
#define YY_SIZEOF 64
#define YY__ALIGNOF 65
#define YY___ALIGNOF__ 66
#define YY_ID 67
#define YY_OCTNUMBER 68
#define YY_DECNUMBER 69
#define YY_HEXNUMBER 70
#define YY_FLOATNUMBER 71
#define YY_STRING 72
#define YY_CHARACTER 73
#define YY_EOL 74
#define YY_WS 75
#define YY_ONE_LINE_COMMENT 76
#define YY_COMMENT 77

const char * sym_name[] = {
	"<EOF>",
	",",
	";",
	"typedef",
	"extern",
	"static",
	"auto",
	"register",
	"inline",
	"_Noreturn",
	"_Alignas",
	"(",
	")",
	"const",
	"restrict",
	"volatile",
	"_Atomic",
	"void",
	"char",
	"short",
	"int",
	"long",
	"float",
	"double",
	"signed",
	"unsigned",
	"_Bool",
	"_Complex",
	"struct",
	"union",
	"{",
	"}",
	":",
	"enum",
	"=",
	"*",
	"[",
	"]",
	"...",
	"__attribute__",
	".",
	"?",
	"||",
	"&&",
	"|",
	"^",
	"&",
	"==",
	"!=",
	"<",
	">",
	"<=",
	">=",
	"<<",
	">>",
	"+",
	"-",
	"/",
	"%",
	"->",
	"++",
	"--",
	"~",
	"!",
	"sizeof",
	"_Alignof",
	"__alignof__",
	"<ID>",
	"<OCTNUMBER>",
	"<DECNUMBER>",
	"<HEXNUMBER>",
	"<FLOATNUMBER>",
	"<STRING>",
	"<CHARACTER>",
	"<EOL>",
	"<WS>",
	"<ONE_LINE_COMMENT>",
	"<COMMENT>",
	NULL
};

#define YY_IN_SET(sym, set, bitset) \
	(bitset[sym>>3] & (1 << (sym & 0x7)))

static int skip_EOL(int sym);
static int skip_WS(int sym);
static int skip_ONE_LINE_COMMENT(int sym);
static int skip_COMMENT(int sym);
static int get_sym(void);
static int parse_declarations(int sym);
static int parse_declaration_specifiers(int sym, zend_ffi_dcl *dcl);
static int parse_specifier_qualifier_list(int sym, zend_ffi_dcl *dcl);
static int parse_type_qualifier_list(int sym, zend_ffi_dcl *dcl);
static int parse_type_qualifier(int sym, zend_ffi_dcl *dcl);
static int parse_type_specifier(int sym, zend_ffi_dcl *dcl);
static int parse_struct_or_union_specifier(int sym, zend_ffi_dcl *dcl);
static int parse_struct_declaration(int sym, zend_ffi_dcl *struct_dcl);
static int parse_struct_declarator(int sym, zend_ffi_dcl *struct_dcl, zend_ffi_dcl *field_dcl);
static int parse_enum_specifier(int sym, zend_ffi_dcl *dcl);
static int parse_enumerator_list(int sym, zend_ffi_dcl *enum_dcl);
static int parse_enumerator(int sym, zend_ffi_dcl *enum_dcl, int64_t *min, int64_t *max, int64_t *last);
static int parse_declarator(int sym, zend_ffi_dcl *dcl, const char **name, size_t *name_len);
static int parse_abstract_declarator(int sym, zend_ffi_dcl *dcl, const char **name, size_t *name_len);
static int parse_pointer(int sym, zend_ffi_dcl *dcl);
static int parse_array_or_function_declarators(int sym, zend_ffi_dcl *dcl);
static int parse_parameter_declaration(int sym, HashTable **args);
static int parse_type_name(int sym, zend_ffi_dcl *dcl);
static int parse_attributes(int sym, zend_ffi_dcl *dcl);
static int parse_attrib(int sym, zend_ffi_dcl *dcl);
static int parse_initializer(int sym);
static int parse_designation(int sym);
static int parse_expr_list(int sym);
static int parse_expression(int sym, zend_ffi_val *val);
static int parse_assignment_expression(int sym, zend_ffi_val *val);
static int parse_constant_expression(int sym, zend_ffi_val *val);
static int parse_conditional_expression(int sym, zend_ffi_val *val);
static int parse_logical_or_expression(int sym, zend_ffi_val *val);
static int parse_logical_and_expression(int sym, zend_ffi_val *val);
static int parse_inclusive_or_expression(int sym, zend_ffi_val *val);
static int parse_exclusive_or_expression(int sym, zend_ffi_val *val);
static int parse_and_expression(int sym, zend_ffi_val *val);
static int parse_equality_expression(int sym, zend_ffi_val *val);
static int parse_relational_expression(int sym, zend_ffi_val *val);
static int parse_shift_expression(int sym, zend_ffi_val *val);
static int parse_additive_expression(int sym, zend_ffi_val *val);
static int parse_multiplicative_expression(int sym, zend_ffi_val *val);
static int parse_cast_expression(int sym, zend_ffi_val *val);
static int parse_unary_expression(int sym, zend_ffi_val *val);
static int parse_ID(int sym, const char **name, size_t *name_len);
static int parse_OCTNUMBER(int sym, zend_ffi_val *val);
static int parse_DECNUMBER(int sym, zend_ffi_val *val);
static int parse_HEXNUMBER(int sym, zend_ffi_val *val);
static int parse_FLOATNUMBER(int sym, zend_ffi_val *val);
static int parse_STRING(int sym, zend_ffi_val *val);
static int parse_CHARACTER(int sym, zend_ffi_val *val);

static int get_skip_sym(void) {
	int ch;
	int ret;
	int accept = -1;
	const unsigned char *accept_pos;
	const unsigned char *cpos = yy_pos;
	const unsigned char *cend = yy_end;

_yy_state_start:
	yy_text = YYPOS;
	ch = *YYPOS;
	switch (ch) {
		case 't':
			ch = *++YYPOS;
			if (ch != 'y') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'p') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'e') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'd') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'e') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'f') goto _yy_tunnel_15;
			ret = YY_TYPEDEF;
			goto _yy_state_281;
		case 'e':
			ch = *++YYPOS;
			if (ch == 'x') {
				ch = *++YYPOS;
				if (ch != 't') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'e') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'r') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'n') goto _yy_tunnel_15;
				ret = YY_EXTERN;
				goto _yy_state_281;
			} else if (ch == 'n') {
				ch = *++YYPOS;
				if (ch != 'u') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'm') goto _yy_tunnel_15;
				ret = YY_ENUM;
				goto _yy_state_281;
			} else {
				goto _yy_tunnel_15;
			}
		case 's':
			ch = *++YYPOS;
			if (ch == 't') {
				ch = *++YYPOS;
				if (ch == 'a') {
					ch = *++YYPOS;
					if (ch != 't') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'i') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'c') goto _yy_tunnel_15;
					ret = YY_STATIC;
					goto _yy_state_281;
				} else if (ch == 'r') {
					ch = *++YYPOS;
					if (ch != 'u') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'c') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 't') goto _yy_tunnel_15;
					ret = YY_STRUCT;
					goto _yy_state_281;
				} else {
					goto _yy_tunnel_15;
				}
			} else if (ch == 'h') {
				ch = *++YYPOS;
				if (ch != 'o') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'r') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 't') goto _yy_tunnel_15;
				ret = YY_SHORT;
				goto _yy_state_281;
			} else if (ch == 'i') {
				ch = *++YYPOS;
				if (ch == 'g') {
					ch = *++YYPOS;
					if (ch != 'n') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'e') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'd') goto _yy_tunnel_15;
					ret = YY_SIGNED;
					goto _yy_state_281;
				} else if (ch == 'z') {
					ch = *++YYPOS;
					if (ch != 'e') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'o') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'f') goto _yy_tunnel_15;
					ret = YY_SIZEOF;
					goto _yy_state_281;
				} else {
					goto _yy_tunnel_15;
				}
			} else {
				goto _yy_tunnel_15;
			}
		case 'a':
			ch = *++YYPOS;
			if (ch != 'u') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 't') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'o') goto _yy_tunnel_15;
			ret = YY_AUTO;
			goto _yy_state_281;
		case 'r':
			ch = *++YYPOS;
			if (ch != 'e') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch == 'g') {
				ch = *++YYPOS;
				if (ch != 'i') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 's') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 't') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'e') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'r') goto _yy_tunnel_15;
				ret = YY_REGISTER;
				goto _yy_state_281;
			} else if (ch == 's') {
				ch = *++YYPOS;
				if (ch != 't') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'r') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'i') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'c') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 't') goto _yy_tunnel_15;
				ret = YY_RESTRICT;
				goto _yy_state_281;
			} else {
				goto _yy_tunnel_15;
			}
		case 'i':
			ch = *++YYPOS;
			if (ch != 'n') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch == 'l') {
				ch = *++YYPOS;
				if (ch != 'i') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'n') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'e') goto _yy_tunnel_15;
				ret = YY_INLINE;
				goto _yy_state_281;
			} else if (ch == 't') {
				ret = YY_INT;
				goto _yy_state_281;
			} else {
				goto _yy_tunnel_15;
			}
		case '_':
			ch = *++YYPOS;
			switch (ch) {
				case 'N':
					ch = *++YYPOS;
					if (ch != 'o') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'r') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'e') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 't') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'u') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'r') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'n') goto _yy_tunnel_15;
					ret = YY__NORETURN;
					goto _yy_state_281;
				case 'A':
					ch = *++YYPOS;
					if (ch == 'l') {
						ch = *++YYPOS;
						if (ch != 'i') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'g') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'n') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch == 'a') {
							ch = *++YYPOS;
							if (ch != 's') goto _yy_tunnel_15;
							ret = YY__ALIGNAS;
							goto _yy_state_281;
						} else if (ch == 'o') {
							ch = *++YYPOS;
							if (ch != 'f') goto _yy_tunnel_15;
							ret = YY__ALIGNOF;
							goto _yy_state_281;
						} else {
							goto _yy_tunnel_15;
						}
					} else if (ch == 't') {
						ch = *++YYPOS;
						if (ch != 'o') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'm') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'i') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'c') goto _yy_tunnel_15;
						ret = YY__ATOMIC;
						goto _yy_state_281;
					} else {
						goto _yy_tunnel_15;
					}
				case 'B':
					ch = *++YYPOS;
					if (ch != 'o') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'o') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'l') goto _yy_tunnel_15;
					ret = YY__BOOL;
					goto _yy_state_281;
				case 'C':
					ch = *++YYPOS;
					if (ch != 'o') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'm') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'p') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'l') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'e') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'x') goto _yy_tunnel_15;
					ret = YY__COMPLEX;
					goto _yy_state_281;
				case '_':
					ch = *++YYPOS;
					if (ch != 'a') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch == 't') {
						ch = *++YYPOS;
						if (ch != 't') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'r') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'i') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'b') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'u') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 't') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'e') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != '_') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != '_') goto _yy_tunnel_15;
						ret = YY___ATTRIBUTE__;
						goto _yy_state_281;
					} else if (ch == 'l') {
						ch = *++YYPOS;
						if (ch != 'i') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'g') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'n') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'o') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != 'f') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != '_') goto _yy_tunnel_15;
						ch = *++YYPOS;
						if (ch != '_') goto _yy_tunnel_15;
						ret = YY___ALIGNOF__;
						goto _yy_state_281;
					} else {
						goto _yy_tunnel_15;
					}
				default:
					goto _yy_tunnel_15;
			}
		case '(':
			YYPOS++;
			ret = YY__LPAREN;
			goto _yy_fin;
		case 'v':
			ch = *++YYPOS;
			if (ch != 'o') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch == 'i') {
				ch = *++YYPOS;
				if (ch != 'd') goto _yy_tunnel_15;
				ret = YY_VOID;
				goto _yy_state_281;
			} else if (ch == 'l') {
				ch = *++YYPOS;
				if (ch != 'a') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 't') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'i') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'l') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'e') goto _yy_tunnel_15;
				ret = YY_VOLATILE;
				goto _yy_state_281;
			} else {
				goto _yy_tunnel_15;
			}
		case 'c':
			ch = *++YYPOS;
			if (ch == 'h') {
				ch = *++YYPOS;
				if (ch != 'a') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 'r') goto _yy_tunnel_15;
				ret = YY_CHAR;
				goto _yy_state_281;
			} else if (ch == 'o') {
				ch = *++YYPOS;
				if (ch != 'n') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 's') goto _yy_tunnel_15;
				ch = *++YYPOS;
				if (ch != 't') goto _yy_tunnel_15;
				ret = YY_CONST;
				goto _yy_state_281;
			} else {
				goto _yy_tunnel_15;
			}
		case 'l':
			ch = *++YYPOS;
			if (ch != 'o') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'n') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'g') goto _yy_tunnel_15;
			ret = YY_LONG;
			goto _yy_state_281;
		case 'f':
			ch = *++YYPOS;
			if (ch != 'l') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'o') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'a') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 't') goto _yy_tunnel_15;
			ret = YY_FLOAT;
			goto _yy_state_281;
		case 'd':
			ch = *++YYPOS;
			if (ch != 'o') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'u') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'b') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'l') goto _yy_tunnel_15;
			ch = *++YYPOS;
			if (ch != 'e') goto _yy_tunnel_15;
			ret = YY_DOUBLE;
			goto _yy_state_281;
		case 'u':
			ch = *++YYPOS;
			if (ch == 'n') {
				ch = *++YYPOS;
				if (ch == 's') {
					ch = *++YYPOS;
					if (ch != 'i') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'g') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'n') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'e') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'd') goto _yy_tunnel_15;
					ret = YY_UNSIGNED;
					goto _yy_state_281;
				} else if (ch == 'i') {
					ch = *++YYPOS;
					if (ch != 'o') goto _yy_tunnel_15;
					ch = *++YYPOS;
					if (ch != 'n') goto _yy_tunnel_15;
					ret = YY_UNION;
					goto _yy_state_281;
				} else {
					goto _yy_tunnel_15;
				}
			} else if (ch == '8') {
				ch = *++YYPOS;
				if (ch != '"') goto _yy_tunnel_15;
				goto _yy_state_27;
			} else if (ch == '"') {
				goto _yy_state_27;
			} else if (ch == '\'') {
				goto _yy_state_28;
			} else {
				goto _yy_tunnel_15;
			}
		case 'A':
		case 'B':
		case 'C':
		case 'D':
		case 'E':
		case 'F':
		case 'G':
		case 'H':
		case 'I':
		case 'J':
		case 'K':
		case 'M':
		case 'N':
		case 'O':
		case 'P':
		case 'Q':
		case 'R':
		case 'S':
		case 'T':
		case 'V':
		case 'W':
		case 'X':
		case 'Y':
		case 'Z':
		case 'b':
		case 'g':
		case 'h':
		case 'j':
		case 'k':
		case 'm':
		case 'n':
		case 'o':
		case 'p':
		case 'q':
		case 'w':
		case 'x':
		case 'y':
		case 'z':
			goto _yy_state_15;
		case 'L':
		case 'U':
			ch = *++YYPOS;
			if (ch == '"') {
				goto _yy_state_27;
			} else if (ch == '\'') {
				goto _yy_state_28;
			} else {
				goto _yy_tunnel_15;
			}
		case ')':
			YYPOS++;
			ret = YY__RPAREN;
			goto _yy_fin;
		case '[':
			YYPOS++;
			ret = YY__LBRACK;
			goto _yy_fin;
		case ',':
			YYPOS++;
			ret = YY__COMMA;
			goto _yy_fin;
		case ']':
			YYPOS++;
			ret = YY__RBRACK;
			goto _yy_fin;
		case '.':
			ch = *++YYPOS;
			accept = YY__POINT;
			accept_pos = yy_pos;
			if ((ch >= '0' && ch <= '9')) {
				goto _yy_state_73;
			} else if (ch == '.') {
				ch = *++YYPOS;
				if (ch == '.') {
					YYPOS++;
					ret = YY__POINT_POINT_POINT;
					goto _yy_fin;
				} else {
					goto _yy_state_error;
				}
			} else {
				ret = YY__POINT;
				goto _yy_fin;
			}
		case '-':
			ch = *++YYPOS;
			if (ch == '>') {
				YYPOS++;
				ret = YY__MINUS_GREATER;
				goto _yy_fin;
			} else if (ch == '-') {
				YYPOS++;
				ret = YY__MINUS_MINUS;
				goto _yy_fin;
			} else {
				ret = YY__MINUS;
				goto _yy_fin;
			}
		case '+':
			ch = *++YYPOS;
			if (ch == '+') {
				YYPOS++;
				ret = YY__PLUS_PLUS;
				goto _yy_fin;
			} else {
				ret = YY__PLUS;
				goto _yy_fin;
			}
		case '0':
			ch = *++YYPOS;
			if (ch != 'X' && ch != 'x') goto _yy_tunnel_78;
			ch = *++YYPOS;
			if ((ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'F') || (ch >= 'a' && ch <= 'f')) {
				goto _yy_state_146;
			} else {
				goto _yy_state_error;
			}
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			goto _yy_state_26;
		case '"':
			goto _yy_state_27;
		case '\'':
			goto _yy_state_28;
		case '&':
			ch = *++YYPOS;
			if (ch == '&') {
				YYPOS++;
				ret = YY__AND_AND;
				goto _yy_fin;
			} else {
				ret = YY__AND;
				goto _yy_fin;
			}
		case '*':
			YYPOS++;
			ret = YY__STAR;
			goto _yy_fin;
		case '~':
			YYPOS++;
			ret = YY__TILDE;
			goto _yy_fin;
		case '!':
			ch = *++YYPOS;
			if (ch == '=') {
				YYPOS++;
				ret = YY__BANG_EQUAL;
				goto _yy_fin;
			} else {
				ret = YY__BANG;
				goto _yy_fin;
			}
		case '/':
			ch = *++YYPOS;
			accept = YY__SLASH;
			accept_pos = yy_pos;
			if (ch == '*') {
				goto _yy_state_99;
			} else if (ch == '/') {
				goto _yy_state_48;
			} else {
				ret = YY__SLASH;
				goto _yy_fin;
			}
		case '%':
			YYPOS++;
			ret = YY__PERCENT;
			goto _yy_fin;
		case '<':
			ch = *++YYPOS;
			if (ch == '<') {
				YYPOS++;
				ret = YY__LESS_LESS;
				goto _yy_fin;
			} else if (ch == '=') {
				YYPOS++;
				ret = YY__LESS_EQUAL;
				goto _yy_fin;
			} else {
				ret = YY__LESS;
				goto _yy_fin;
			}
		case '>':
			ch = *++YYPOS;
			if (ch == '>') {
				YYPOS++;
				ret = YY__GREATER_GREATER;
				goto _yy_fin;
			} else if (ch == '=') {
				YYPOS++;
				ret = YY__GREATER_EQUAL;
				goto _yy_fin;
			} else {
				ret = YY__GREATER;
				goto _yy_fin;
			}
		case '=':
			ch = *++YYPOS;
			if (ch == '=') {
				YYPOS++;
				ret = YY__EQUAL_EQUAL;
				goto _yy_fin;
			} else {
				ret = YY__EQUAL;
				goto _yy_fin;
			}
		case '^':
			YYPOS++;
			ret = YY__UPARROW;
			goto _yy_fin;
		case '|':
			ch = *++YYPOS;
			if (ch == '|') {
				YYPOS++;
				ret = YY__BAR_BAR;
				goto _yy_fin;
			} else {
				ret = YY__BAR;
				goto _yy_fin;
			}
		case '?':
			YYPOS++;
			ret = YY__QUERY;
			goto _yy_fin;
		case ':':
			YYPOS++;
			ret = YY__COLON;
			goto _yy_fin;
		case '{':
			YYPOS++;
			ret = YY__LBRACE;
			goto _yy_fin;
		case ';':
			YYPOS++;
			ret = YY__SEMICOLON;
			goto _yy_fin;
		case '}':
			YYPOS++;
			ret = YY__RBRACE;
			goto _yy_fin;
		case '\r':
			ch = *++YYPOS;
			if (ch == '\n') {
				yy_line++;
				YYPOS++;
				ret = YY_EOL;
				goto _yy_fin;
			} else {
				ret = YY_EOL;
				goto _yy_fin;
			}
		case '\n':
			yy_line++;
			YYPOS++;
			ret = YY_EOL;
			goto _yy_fin;
		case ' ':
		case '\t':
		case '\f':
		case '\v':
			goto _yy_state_47;
		case '#':
			goto _yy_state_48;
		case '\0':
			if (ch == 0 && YYPOS < YYEND) goto _yy_state_error;
			YYPOS++;
			ret = YY_EOF;
			goto _yy_fin;
		default:
			goto _yy_state_error;
	}
_yy_state_15:
	ch = *++YYPOS;
_yy_tunnel_15:
	if ((ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'Z') || ch == '_' || (ch >= 'a' && ch <= 'z')) {
		goto _yy_state_15;
	} else {
		ret = YY_ID;
		goto _yy_fin;
	}
_yy_state_26:
	ch = *++YYPOS;
	accept = YY_DECNUMBER;
	accept_pos = yy_pos;
	switch (ch) {
		case 'U':
		case 'u':
			ch = *++YYPOS;
			if (ch == 'L') {
				ch = *++YYPOS;
				if (ch == 'L') {
					YYPOS++;
					ret = YY_DECNUMBER;
					goto _yy_fin;
				} else {
					ret = YY_DECNUMBER;
					goto _yy_fin;
				}
			} else if (ch == 'l') {
				YYPOS++;
				ret = YY_DECNUMBER;
				goto _yy_fin;
			} else {
				ret = YY_DECNUMBER;
				goto _yy_fin;
			}
		case 'L':
			ch = *++YYPOS;
			accept = YY_DECNUMBER;
			accept_pos = yy_pos;
			if (ch == 'L') {
				goto _yy_state_153;
			} else if (ch == 'U' || ch == 'u') {
				YYPOS++;
				ret = YY_DECNUMBER;
				goto _yy_fin;
			} else {
				ret = YY_DECNUMBER;
				goto _yy_fin;
			}
		case 'l':
			ch = *++YYPOS;
			accept = YY_DECNUMBER;
			accept_pos = yy_pos;
			if (ch == 'U' || ch == 'u') {
				YYPOS++;
				ret = YY_DECNUMBER;
				goto _yy_fin;
			} else if (ch == 'l') {
				goto _yy_state_153;
			} else {
				ret = YY_DECNUMBER;
				goto _yy_fin;
			}
		case 'E':
		case 'e':
			goto _yy_state_85;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			goto _yy_state_26;
		case '.':
			goto _yy_state_73;
		default:
			ret = YY_DECNUMBER;
			goto _yy_fin;
	}
_yy_state_27:
	ch = *++YYPOS;
	if (ch == '\\') {
		ch = *++YYPOS;
		if (YYPOS < YYEND) {
			if (ch == '\n') {
				yy_line++;
			}
			goto _yy_state_27;
		} else {
			goto _yy_state_error;
		}
	} else if (ch == '"') {
		YYPOS++;
		ret = YY_STRING;
		goto _yy_fin;
	} else if (YYPOS < YYEND && (ch <= '!' || (ch >= '#' && ch <= '[') || ch >= ']')) {
		if (ch == '\n') {
			yy_line++;
		}
		goto _yy_state_27;
	} else {
		goto _yy_state_error;
	}
_yy_state_28:
	ch = *++YYPOS;
	if (ch == '\\') {
		ch = *++YYPOS;
		if (YYPOS < YYEND) {
			if (ch == '\n') {
				yy_line++;
			}
			goto _yy_state_28;
		} else {
			goto _yy_state_error;
		}
	} else if (ch == '\'') {
		YYPOS++;
		ret = YY_CHARACTER;
		goto _yy_fin;
	} else if (YYPOS < YYEND && (ch <= '&' || (ch >= '(' && ch <= '[') || ch >= ']')) {
		if (ch == '\n') {
			yy_line++;
		}
		goto _yy_state_28;
	} else {
		goto _yy_state_error;
	}
_yy_state_47:
	ch = *++YYPOS;
	if (ch == '\t' || ch == '\v' || ch == '\f' || ch == ' ') {
		goto _yy_state_47;
	} else {
		ret = YY_WS;
		goto _yy_fin;
	}
_yy_state_48:
	ch = *++YYPOS;
	if (ch == '\r') {
		ch = *++YYPOS;
		if (ch == '\n') {
			yy_line++;
			YYPOS++;
			ret = YY_ONE_LINE_COMMENT;
			goto _yy_fin;
		} else {
			ret = YY_ONE_LINE_COMMENT;
			goto _yy_fin;
		}
	} else if (ch == '\n') {
		yy_line++;
		YYPOS++;
		ret = YY_ONE_LINE_COMMENT;
		goto _yy_fin;
	} else if (YYPOS < YYEND && (ch <= '\t' || ch == '\v' || ch == '\f' || ch >= '\016')) {
		goto _yy_state_48;
	} else {
		goto _yy_state_error;
	}
_yy_state_73:
	ch = *++YYPOS;
	accept = YY_FLOATNUMBER;
	accept_pos = yy_pos;
	if ((ch >= '0' && ch <= '9')) {
		goto _yy_state_73;
	} else if (ch == 'F' || ch == 'L' || ch == 'f' || ch == 'l') {
		YYPOS++;
		ret = YY_FLOATNUMBER;
		goto _yy_fin;
	} else if (ch == 'E' || ch == 'e') {
		goto _yy_state_85;
	} else {
		ret = YY_FLOATNUMBER;
		goto _yy_fin;
	}
_yy_state_78:
	ch = *++YYPOS;
_yy_tunnel_78:
	accept = YY_OCTNUMBER;
	accept_pos = yy_pos;
	switch (ch) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
			goto _yy_state_78;
		case 'U':
		case 'u':
			ch = *++YYPOS;
			if (ch == 'L') {
				ch = *++YYPOS;
				if (ch == 'L') {
					YYPOS++;
					ret = YY_OCTNUMBER;
					goto _yy_fin;
				} else {
					ret = YY_OCTNUMBER;
					goto _yy_fin;
				}
			} else if (ch == 'l') {
				YYPOS++;
				ret = YY_OCTNUMBER;
				goto _yy_fin;
			} else {
				ret = YY_OCTNUMBER;
				goto _yy_fin;
			}
		case 'L':
			ch = *++YYPOS;
			accept = YY_OCTNUMBER;
			accept_pos = yy_pos;
			if (ch == 'L') {
				goto _yy_state_144;
			} else if (ch == 'U' || ch == 'u') {
				YYPOS++;
				ret = YY_OCTNUMBER;
				goto _yy_fin;
			} else {
				ret = YY_OCTNUMBER;
				goto _yy_fin;
			}
		case 'l':
			ch = *++YYPOS;
			accept = YY_OCTNUMBER;
			accept_pos = yy_pos;
			if (ch == 'l') {
				goto _yy_state_144;
			} else if (ch == 'U' || ch == 'u') {
				YYPOS++;
				ret = YY_OCTNUMBER;
				goto _yy_fin;
			} else {
				ret = YY_OCTNUMBER;
				goto _yy_fin;
			}
		case '8':
		case '9':
			goto _yy_state_83;
		case 'E':
		case 'e':
			goto _yy_state_85;
		case '.':
			goto _yy_state_73;
		default:
			ret = YY_OCTNUMBER;
			goto _yy_fin;
	}
_yy_state_83:
	ch = *++YYPOS;
	if ((ch >= '0' && ch <= '9')) {
		goto _yy_state_83;
	} else if (ch == 'E' || ch == 'e') {
		goto _yy_state_85;
	} else if (ch == '.') {
		goto _yy_state_73;
	} else {
		goto _yy_state_error;
	}
_yy_state_85:
	ch = *++YYPOS;
	if (ch == '+' || ch == '-') {
		ch = *++YYPOS;
		if ((ch >= '0' && ch <= '9')) {
			goto _yy_state_149;
		} else {
			goto _yy_state_error;
		}
	} else if ((ch >= '0' && ch <= '9')) {
		goto _yy_state_149;
	} else {
		goto _yy_state_error;
	}
_yy_state_99:
	ch = *++YYPOS;
_yy_tunnel_99:
	if (ch == '*') {
		ch = *++YYPOS;
		if (ch != '/') goto _yy_tunnel_99;
		YYPOS++;
		ret = YY_COMMENT;
		goto _yy_fin;
	} else if (YYPOS < YYEND && (ch <= ')' || ch >= '+')) {
		if (ch == '\n') {
			yy_line++;
		}
		goto _yy_state_99;
	} else {
		goto _yy_state_error;
	}
_yy_state_144:
	ch = *++YYPOS;
	if (ch == 'U' || ch == 'u') {
		YYPOS++;
		ret = YY_OCTNUMBER;
		goto _yy_fin;
	} else {
		goto _yy_state_error;
	}
_yy_state_146:
	ch = *++YYPOS;
	if (ch == 'U' || ch == 'u') {
		ch = *++YYPOS;
		if (ch == 'L') {
			ch = *++YYPOS;
			if (ch == 'L') {
				YYPOS++;
				ret = YY_HEXNUMBER;
				goto _yy_fin;
			} else {
				ret = YY_HEXNUMBER;
				goto _yy_fin;
			}
		} else if (ch == 'l') {
			YYPOS++;
			ret = YY_HEXNUMBER;
			goto _yy_fin;
		} else {
			ret = YY_HEXNUMBER;
			goto _yy_fin;
		}
	} else if (ch == 'L') {
		ch = *++YYPOS;
		accept = YY_HEXNUMBER;
		accept_pos = yy_pos;
		if (ch == 'L') {
			goto _yy_state_228;
		} else if (ch == 'U' || ch == 'u') {
			YYPOS++;
			ret = YY_HEXNUMBER;
			goto _yy_fin;
		} else {
			ret = YY_HEXNUMBER;
			goto _yy_fin;
		}
	} else if (ch == 'l') {
		ch = *++YYPOS;
		accept = YY_HEXNUMBER;
		accept_pos = yy_pos;
		if (ch == 'U' || ch == 'u') {
			YYPOS++;
			ret = YY_HEXNUMBER;
			goto _yy_fin;
		} else if (ch == 'l') {
			goto _yy_state_228;
		} else {
			ret = YY_HEXNUMBER;
			goto _yy_fin;
		}
	} else if ((ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'F') || (ch >= 'a' && ch <= 'f')) {
		goto _yy_state_146;
	} else {
		ret = YY_HEXNUMBER;
		goto _yy_fin;
	}
_yy_state_149:
	ch = *++YYPOS;
	if ((ch >= '0' && ch <= '9')) {
		goto _yy_state_149;
	} else if (ch == 'F' || ch == 'L' || ch == 'f' || ch == 'l') {
		YYPOS++;
		ret = YY_FLOATNUMBER;
		goto _yy_fin;
	} else {
		ret = YY_FLOATNUMBER;
		goto _yy_fin;
	}
_yy_state_153:
	ch = *++YYPOS;
	if (ch == 'U' || ch == 'u') {
		YYPOS++;
		ret = YY_DECNUMBER;
		goto _yy_fin;
	} else {
		goto _yy_state_error;
	}
_yy_state_228:
	ch = *++YYPOS;
	if (ch == 'U' || ch == 'u') {
		YYPOS++;
		ret = YY_HEXNUMBER;
		goto _yy_fin;
	} else {
		goto _yy_state_error;
	}
_yy_state_281:
	ch = *++YYPOS;
_yy_tunnel_281:
	if ((ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'Z') || ch == '_' || (ch >= 'a' && ch <= 'z')) {
		goto _yy_state_15;
	} else {
		goto _yy_fin;
	}
_yy_state_error:
	if (accept >= 0) {
		yy_pos = accept_pos;
		return accept;
	}
	if (YYPOS >= YYEND) {
		yy_error("Unexpected <EOF>");
	} else if (YYPOS == yy_text) {
		yy_error("Unexpected character 'escape_char(ch)'");
	} else {
		yy_error("Unexpected sequence 'escape_string(yy_text, 1 + YYPOS - yy_text))'");
	}
	YYPOS++;
	goto _yy_state_start;
_yy_fin:
	yy_pos = YYPOS;
	return ret;
}

static int skip_EOL(int sym) {
	if (sym != YY_EOL) {
		yy_error_sym("<EOL> expected, got '%s'", sym);
	}
	sym = get_skip_sym();
	return sym;
}

static int skip_WS(int sym) {
	if (sym != YY_WS) {
		yy_error_sym("<WS> expected, got '%s'", sym);
	}
	sym = get_skip_sym();
	return sym;
}

static int skip_ONE_LINE_COMMENT(int sym) {
	if (sym != YY_ONE_LINE_COMMENT) {
		yy_error_sym("<ONE_LINE_COMMENT> expected, got '%s'", sym);
	}
	sym = get_skip_sym();
	return sym;
}

static int skip_COMMENT(int sym) {
	if (sym != YY_COMMENT) {
		yy_error_sym("<COMMENT> expected, got '%s'", sym);
	}
	sym = get_skip_sym();
	return sym;
}

static int get_sym(void) {
	int sym;
	sym = get_skip_sym();
	while (sym == YY_EOL || sym == YY_WS || sym == YY_ONE_LINE_COMMENT || sym == YY_COMMENT) {
		if (sym == YY_EOL) {
			sym = skip_EOL(sym);
		} else if (sym == YY_WS) {
			sym = skip_WS(sym);
		} else if (sym == YY_ONE_LINE_COMMENT) {
			sym = skip_ONE_LINE_COMMENT(sym);
		} else if (sym == YY_COMMENT) {
			sym = skip_COMMENT(sym);
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	}
	return sym;
}

static int parse_declarations(int sym) {
	while (YY_IN_SET(sym, (YY_TYPEDEF,YY_EXTERN,YY_STATIC,YY_AUTO,YY_REGISTER,YY_INLINE,YY__NORETURN,YY__ALIGNAS,YY___ATTRIBUTE__,YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID), "\370\347\377\077\202\000\000\000\010\000")) {
		zend_ffi_dcl common_dcl = {0, 0, 0, NULL};
		sym = parse_declaration_specifiers(sym, &common_dcl);
		if (sym == YY__STAR || sym == YY_ID || sym == YY__LPAREN) {
			const char *name;
			size_t name_len;
			zend_ffi_dcl dcl;
			dcl = common_dcl;
			sym = parse_declarator(sym, &dcl, &name, &name_len);
			if (sym == YY___ATTRIBUTE__) {
				sym = parse_attributes(sym, &dcl);
			}
			if (sym == YY__EQUAL) {
				sym = parse_initializer(sym);
			}
			zend_ffi_declare(name, name_len, &dcl);
			while (sym == YY__COMMA) {
				sym = get_sym();
				dcl = common_dcl;
				sym = parse_declarator(sym, &dcl, &name, &name_len);
				if (sym == YY___ATTRIBUTE__) {
					sym = parse_attributes(sym, &dcl);
				}
				if (sym == YY__EQUAL) {
					sym = parse_initializer(sym);
				}
				zend_ffi_declare(name, name_len, &dcl);
			}
		}
		if (sym != YY__SEMICOLON) {
			yy_error_sym("';' expected, got '%s'", sym);
		}
		sym = get_sym();
	}
	return sym;
}

static int parse_declaration_specifiers(int sym, zend_ffi_dcl *dcl) {
	do {
		switch (sym) {
			case YY_TYPEDEF:
				if (dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) yy_error_sym("Unexpected '%s'", sym);
				sym = get_sym();
				dcl->flags |= ZEND_FFI_DCL_TYPEDEF;
				break;
			case YY_EXTERN:
				if (dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) yy_error_sym("Unexpected '%s'", sym);
				sym = get_sym();
				dcl->flags |= ZEND_FFI_DCL_EXTERN;
				break;
			case YY_STATIC:
				if (dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) yy_error_sym("Unexpected '%s'", sym);
				sym = get_sym();
				dcl->flags |= ZEND_FFI_DCL_STATIC;
				break;
			case YY_AUTO:
				if (dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) yy_error_sym("Unexpected '%s'", sym);
				sym = get_sym();
				dcl->flags |= ZEND_FFI_DCL_AUTO;
				break;
			case YY_REGISTER:
				if (dcl->flags & ZEND_FFI_DCL_STORAGE_CLASS) yy_error_sym("Unexpected '%s'", sym);
				sym = get_sym();
				dcl->flags |= ZEND_FFI_DCL_REGISTER;
				break;
			case YY_INLINE:
				sym = get_sym();
				dcl->flags |= ZEND_FFI_DCL_INLINE;
				break;
			case YY__NORETURN:
				sym = get_sym();
				dcl->flags |= ZEND_FFI_DCL_NO_RETURN;
				break;
			case YY__ALIGNAS:
				sym = get_sym();
				if (sym != YY__LPAREN) {
					yy_error_sym("'(' expected, got '%s'", sym);
				}
				sym = get_sym();
				if (YY_IN_SET(sym, (YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID,YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\377\077\202\000\000\000\010\000")) {
					zend_ffi_dcl align_dcl = {0, 0, 0, NULL};
					sym = parse_type_name(sym, &align_dcl);
					/*align_as_type???*/
				} else if (YY_IN_SET(sym, (YY__LPAREN,YY_ID,YY_OCTNUMBER,YY_DECNUMBER,YY_HEXNUMBER,YY_FLOATNUMBER,YY_STRING,YY_CHARACTER,YY__PLUS_PLUS,YY__MINUS_MINUS,YY__AND,YY__STAR,YY__PLUS,YY__MINUS,YY__TILDE,YY__BANG,YY_SIZEOF,YY__ALIGNOF,YY___ALIGNOF__), "\000\010\000\000\010\100\200\361\377\003")) {
					zend_ffi_val align_val;
					sym = parse_constant_expression(sym, &align_val);
					/*align_as_val???*/
				} else {
					yy_error_sym("unexpected '%s'", sym);
				}
				if (sym != YY__RPAREN) {
					yy_error_sym("')' expected, got '%s'", sym);
				}
				sym = get_sym();
				break;
			case YY___ATTRIBUTE__:
				sym = parse_attributes(sym, dcl);
				break;
			case YY_CONST:
			case YY_RESTRICT:
			case YY_VOLATILE:
			case YY__ATOMIC:
				sym = parse_type_qualifier(sym, dcl);
				break;
			case YY_VOID:
			case YY_CHAR:
			case YY_SHORT:
			case YY_INT:
			case YY_LONG:
			case YY_FLOAT:
			case YY_DOUBLE:
			case YY_SIGNED:
			case YY_UNSIGNED:
			case YY__BOOL:
			case YY__COMPLEX:
			case YY_STRUCT:
			case YY_UNION:
			case YY_ENUM:
			case YY_ID:
				sym = parse_type_specifier(sym, dcl);
				break;
			default:
				yy_error_sym("unexpected '%s'", sym);
		}
	} while (YY_IN_SET(sym, (YY_TYPEDEF,YY_EXTERN,YY_STATIC,YY_AUTO,YY_REGISTER,YY_INLINE,YY__NORETURN,YY__ALIGNAS,YY___ATTRIBUTE__,YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID), "\370\347\377\077\202\000\000\000\010\000") && (sym != YY_ID || zend_ffi_is_typedef_name((const char*)yy_text, yy_pos - yy_text)));
	return sym;
}

static int parse_specifier_qualifier_list(int sym, zend_ffi_dcl *dcl) {
	do {
		if (YY_IN_SET(sym, (YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID), "\000\000\376\077\002\000\000\000\010\000")) {
			sym = parse_type_specifier(sym, dcl);
		} else if (sym == YY_CONST || sym == YY_RESTRICT || sym == YY_VOLATILE || sym == YY__ATOMIC) {
			sym = parse_type_qualifier(sym, dcl);
		} else if (sym == YY___ATTRIBUTE__) {
			sym = parse_attributes(sym, dcl);
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	} while (YY_IN_SET(sym, (YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID,YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\377\077\202\000\000\000\010\000") && (sym != YY_ID || zend_ffi_is_typedef_name((const char*)yy_text, yy_pos - yy_text)));
	return sym;
}

static int parse_type_qualifier_list(int sym, zend_ffi_dcl *dcl) {
	do {
		if (sym == YY_CONST || sym == YY_RESTRICT || sym == YY_VOLATILE || sym == YY__ATOMIC) {
			sym = parse_type_qualifier(sym, dcl);
		} else if (sym == YY___ATTRIBUTE__) {
			sym = parse_attributes(sym, dcl);
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	} while (YY_IN_SET(sym, (YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\001\000\200\000\000\000\000\000"));
	return sym;
}

static int parse_type_qualifier(int sym, zend_ffi_dcl *dcl) {
	if (sym == YY_CONST) {
		sym = get_sym();
		dcl->flags |= ZEND_FFI_DCL_CONST;
	} else if (sym == YY_RESTRICT) {
		sym = get_sym();
		dcl->flags |= ZEND_FFI_DCL_RESTRICT;
	} else if (sym == YY_VOLATILE) {
		sym = get_sym();
		dcl->flags |= ZEND_FFI_DCL_VOLATILE;
	} else if (sym == YY__ATOMIC) {
		sym = get_sym();
		dcl->flags |= ZEND_FFI_DCL_ATOMIC;
	}
	return sym;
}

static int parse_type_specifier(int sym, zend_ffi_dcl *dcl) {
	const char *name;
	size_t name_len;
	switch (sym) {
		case YY_VOID:
			if (dcl->flags & ZEND_FFI_DCL_TYPE_SPECIFIERS) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_VOID;
			break;
		case YY_CHAR:
			if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_SIGNED|ZEND_FFI_DCL_UNSIGNED))) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_CHAR;
			break;
		case YY_SHORT:
			if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_SIGNED|ZEND_FFI_DCL_UNSIGNED|ZEND_FFI_DCL_INT))) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_SHORT;
			break;
		case YY_INT:
			if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_SIGNED|ZEND_FFI_DCL_UNSIGNED|ZEND_FFI_DCL_SHORT|ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_LONG_LONG))) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_INT;
			break;
		case YY_LONG:
			if (dcl->flags & ZEND_FFI_DCL_LONG) {
				if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_SIGNED|ZEND_FFI_DCL_UNSIGNED|ZEND_FFI_DCL_INT))) yy_error_sym("Unexpected '%s'", sym);
				dcl->flags |= ZEND_FFI_DCL_LONG_LONG;
			} else {
				if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_SIGNED|ZEND_FFI_DCL_UNSIGNED|ZEND_FFI_DCL_INT|ZEND_FFI_DCL_DOUBLE|ZEND_FFI_DCL_COMPLEX))) yy_error_sym("Unexpected '%s'", sym);
				dcl->flags |= ZEND_FFI_DCL_LONG;
			}
			sym = get_sym();
			break;
		case YY_FLOAT:
			if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_COMPLEX))) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_FLOAT;
			break;
		case YY_DOUBLE:
			if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_COMPLEX))) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_DOUBLE;
			break;
		case YY_SIGNED:
			if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_CHAR|ZEND_FFI_DCL_SHORT|ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_LONG_LONG|ZEND_FFI_DCL_INT))) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_SIGNED;
			break;
		case YY_UNSIGNED:
			if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_CHAR|ZEND_FFI_DCL_SHORT|ZEND_FFI_DCL_LONG|ZEND_FFI_DCL_LONG_LONG|ZEND_FFI_DCL_INT))) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_UNSIGNED;
			break;
		case YY__BOOL:
			if (dcl->flags & ZEND_FFI_DCL_TYPE_SPECIFIERS) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_BOOL;
			break;
		case YY__COMPLEX:
			if (dcl->flags & (ZEND_FFI_DCL_TYPE_SPECIFIERS-(ZEND_FFI_DCL_FLOAT|ZEND_FFI_DCL_DOUBLE|ZEND_FFI_DCL_LONG))) yy_error_sym("Unexpected '%s'", sym);
			sym = get_sym();
			dcl->flags |= ZEND_FFI_DCL_COMPLEX;
			break;
		case YY_STRUCT:
		case YY_UNION:
			if (dcl->flags & ZEND_FFI_DCL_TYPE_SPECIFIERS) yy_error_sym("Unexpected '%s'", sym);
			sym = parse_struct_or_union_specifier(sym, dcl);
			break;
		case YY_ENUM:
			if (dcl->flags & ZEND_FFI_DCL_TYPE_SPECIFIERS) yy_error_sym("Unexpected '%s'", sym);
			sym = parse_enum_specifier(sym, dcl);
			break;
		case YY_ID:
			if (dcl->flags & ZEND_FFI_DCL_TYPE_SPECIFIERS) yy_error_sym("Unexpected '%s'", sym);
			/*redeclaration of '%.*s' ??? */
			sym = parse_ID(sym, &name, &name_len);
			dcl->flags |= ZEND_FFI_DCL_TYPEDEF_NAME;
			zend_ffi_resolve_typedef(name, name_len, dcl);
			break;
	}
	return sym;
}

static int parse_struct_or_union_specifier(int sym, zend_ffi_dcl *dcl) {
	if (sym == YY_STRUCT) {
		sym = get_sym();
		dcl->flags |= ZEND_FFI_DCL_STRUCT;
	} else if (sym == YY_UNION) {
		sym = get_sym();
		dcl->flags |= ZEND_FFI_DCL_UNION;
	} else {
		yy_error_sym("unexpected '%s'", sym);
	}
	if (sym == YY___ATTRIBUTE__) {
		sym = parse_attributes(sym, dcl);
	}
	if (sym == YY_ID) {
		const char *name;
		size_t name_len;
		sym = parse_ID(sym, &name, &name_len);
		zend_ffi_declare_tag(name, name_len, dcl, 1);
		if (sym == YY__LBRACE) {
			sym = get_sym();
			while (YY_IN_SET(sym, (YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID,YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\377\077\202\000\000\000\010\000")) {
				sym = parse_struct_declaration(sym, dcl);
			}
			if (sym != YY__RBRACE) {
				yy_error_sym("'}' expected, got '%s'", sym);
			}
			sym = get_sym();
			if (sym == YY___ATTRIBUTE__) {
				sym = parse_attributes(sym, dcl);
			}
			zend_ffi_adjust_struct_size(dcl);
			zend_ffi_declare_tag(name, name_len, dcl, 0);
		}
	} else if (sym == YY__LBRACE) {
		sym = get_sym();
		zend_ffi_make_struct_type(dcl);
		while (YY_IN_SET(sym, (YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID,YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\377\077\202\000\000\000\010\000")) {
			sym = parse_struct_declaration(sym, dcl);
		}
		if (sym != YY__RBRACE) {
			yy_error_sym("'}' expected, got '%s'", sym);
		}
		sym = get_sym();
		if (sym == YY___ATTRIBUTE__) {
			sym = parse_attributes(sym, dcl);
		}
		zend_ffi_adjust_struct_size(dcl);
	}
	return sym;
}

static int parse_struct_declaration(int sym, zend_ffi_dcl *struct_dcl) {
	zend_ffi_dcl common_field_dcl = {0, 0, 0, NULL};
	sym = parse_specifier_qualifier_list(sym, &common_field_dcl);
	sym = parse_struct_declarator(sym, struct_dcl, &common_field_dcl);
	while (sym == YY__COMMA) {
		sym = get_sym();
		zend_ffi_dcl field_dcl = common_field_dcl;
		if (sym == YY___ATTRIBUTE__) {
			sym = parse_attributes(sym, &field_dcl);
		}
		sym = parse_struct_declarator(sym, struct_dcl, &field_dcl);
	}
	if (sym != YY__SEMICOLON) {
		yy_error_sym("';' expected, got '%s'", sym);
	}
	sym = get_sym();
	return sym;
}

static int parse_struct_declarator(int sym, zend_ffi_dcl *struct_dcl, zend_ffi_dcl *field_dcl) {
	const char *name;
	size_t name_len;
	zend_ffi_val bits;
	if (sym == YY__STAR || sym == YY_ID || sym == YY__LPAREN) {
		sym = parse_declarator(sym, field_dcl, &name, &name_len);
		if (sym == YY__COLON) {
			sym = get_sym();
			sym = parse_constant_expression(sym, &bits);
			if (sym == YY___ATTRIBUTE__) {
				sym = parse_attributes(sym, field_dcl);
			}
			zend_ffi_add_bit_field(struct_dcl, name, name_len, field_dcl, &bits);
		} else if (sym == YY___ATTRIBUTE__ || sym == YY__COMMA || sym == YY__SEMICOLON) {
			if (sym == YY___ATTRIBUTE__) {
				sym = parse_attributes(sym, field_dcl);
			}
			zend_ffi_add_field(struct_dcl, name, name_len, field_dcl);
		}
	} else if (sym == YY__COLON) {
		sym = get_sym();
		sym = parse_constant_expression(sym, &bits);
		zend_ffi_skip_bit_field(struct_dcl, &bits);
	}
	return sym;
}

static int parse_enum_specifier(int sym, zend_ffi_dcl *dcl) {
	if (sym != YY_ENUM) {
		yy_error_sym("'enum' expected, got '%s'", sym);
	}
	sym = get_sym();
	dcl->flags |= ZEND_FFI_DCL_ENUM;
	if (sym == YY___ATTRIBUTE__) {
		sym = parse_attributes(sym, dcl);
	}
	if (sym == YY_ID) {
		const char *name;
		size_t name_len;
		sym = parse_ID(sym, &name, &name_len);
		if (sym == YY__LBRACE) {
			zend_ffi_declare_tag(name, name_len, dcl, 0);
			sym = get_sym();
			sym = parse_enumerator_list(sym, dcl);
			if (sym != YY__RBRACE) {
				yy_error_sym("'}' expected, got '%s'", sym);
			}
			sym = get_sym();
			if (sym == YY___ATTRIBUTE__) {
				sym = parse_attributes(sym, dcl);
			}
		} else {
			zend_ffi_declare_tag(name, name_len, dcl, 1);
		}
	} else if (sym == YY__LBRACE) {
		sym = get_sym();
		zend_ffi_make_enum_type(dcl);
		sym = parse_enumerator_list(sym, dcl);
		if (sym != YY__RBRACE) {
			yy_error_sym("'}' expected, got '%s'", sym);
		}
		sym = get_sym();
		if (sym == YY___ATTRIBUTE__) {
			sym = parse_attributes(sym, dcl);
		}
	}
	return sym;
}

static int parse_enumerator_list(int sym, zend_ffi_dcl *enum_dcl) {
	int   sym2;
	const unsigned char *save_pos;
	const unsigned char *save_text;
	int   save_line;
	int alt282;
	int64_t min = 0, max = 0, last = -1;
	sym = parse_enumerator(sym, enum_dcl, &min, &max, &last);
	while (1) {
		save_pos  = yy_pos;
		save_text = yy_text;
		save_line = yy_line;
		alt282 = -2;
		sym2 = sym;
		if (sym2 == YY__COMMA) {
			sym2 = get_sym();
			goto _yy_state_282_1;
		} else if (sym2 == YY__RBRACE) {
			alt282 = -1;
			goto _yy_state_282;
		} else {
			yy_error_sym("unexpected '%s'", sym2);
		}
_yy_state_282_1:
		if (sym2 == YY_ID) {
			alt282 = 283;
			goto _yy_state_282;
		} else if (sym2 == YY__RBRACE) {
			alt282 = 285;
			goto _yy_state_282;
		} else {
			yy_error_sym("unexpected '%s'", sym2);
		}
_yy_state_282:
		yy_pos  = save_pos;
		yy_text = save_text;
		yy_line = save_line;
		if (alt282 != 283) {
			break;
		}
		sym = get_sym();
		sym = parse_enumerator(sym, enum_dcl, &min, &max, &last);
	}
	if (alt282 == 285) {
		sym = get_sym();
	}
	return sym;
}

static int parse_enumerator(int sym, zend_ffi_dcl *enum_dcl, int64_t *min, int64_t *max, int64_t *last) {
	const char *name;
	size_t name_len;
	zend_ffi_val val = {.kind = ZEND_FFI_VAL_EMPTY};
	sym = parse_ID(sym, &name, &name_len);
	if (sym == YY__EQUAL) {
		sym = get_sym();
		sym = parse_constant_expression(sym, &val);
	}
	zend_ffi_add_enum_val(enum_dcl, name, name_len, &val, min, max, last);
	return sym;
}

static int parse_declarator(int sym, zend_ffi_dcl *dcl, const char **name, size_t *name_len) {
	zend_ffi_dcl nested_dcl = {ZEND_FFI_DCL_CHAR, 0, 0, NULL};
	zend_bool nested = 0;
	sym = parse_pointer(sym, dcl);
	if (sym == YY_ID) {
		sym = parse_ID(sym, name, name_len);
	} else if (sym == YY__LPAREN) {
		sym = get_sym();
		if (sym == YY___ATTRIBUTE__) {
			sym = parse_attributes(sym, &nested_dcl);
		}
		sym = parse_declarator(sym, &nested_dcl, name, name_len);
		if (sym != YY__RPAREN) {
			yy_error_sym("')' expected, got '%s'", sym);
		}
		sym = get_sym();
		nested = 1;
	} else {
		yy_error_sym("unexpected '%s'", sym);
	}
	sym = parse_array_or_function_declarators(sym, dcl);
	if (nested) zend_ffi_nested_declaration(dcl, &nested_dcl);
	return sym;
}

static int parse_abstract_declarator(int sym, zend_ffi_dcl *dcl, const char **name, size_t *name_len) {
	zend_ffi_dcl nested_dcl = {ZEND_FFI_DCL_CHAR, 0, 0, NULL};
	zend_bool nested = 0;
	sym = parse_pointer(sym, dcl);
	if (sym == YY_ID || sym == YY__LPAREN) {
		if (sym == YY_ID) {
			sym = parse_ID(sym, name, name_len);
		} else if (sym == YY__LPAREN) {
			sym = get_sym();
			if (sym == YY___ATTRIBUTE__) {
				sym = parse_attributes(sym, &nested_dcl);
			}
			sym = parse_abstract_declarator(sym, &nested_dcl, name, name_len);
			if (sym != YY__RPAREN) {
				yy_error_sym("')' expected, got '%s'", sym);
			}
			sym = get_sym();
			nested = 1;
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	}
	sym = parse_array_or_function_declarators(sym, dcl);
	if (nested) zend_ffi_nested_declaration(dcl, &nested_dcl);
	return sym;
}

static int parse_pointer(int sym, zend_ffi_dcl *dcl) {
	while (sym == YY__STAR) {
		sym = get_sym();
		zend_ffi_make_pointer_type(dcl);
		if (YY_IN_SET(sym, (YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\001\000\200\000\000\000\000\000")) {
			sym = parse_type_qualifier_list(sym, dcl);
		}
	}
	return sym;
}

static int parse_array_or_function_declarators(int sym, zend_ffi_dcl *dcl) {
	int   sym2;
	const unsigned char *save_pos;
	const unsigned char *save_text;
	int   save_line;
	int alt239;
	int alt235;
	int alt249;
	zend_ffi_dcl dummy = {0, 0, 0, NULL};
	zend_ffi_val len = {.kind = ZEND_FFI_VAL_EMPTY};
	HashTable *args = NULL;
	zend_bool variadic = 0;
	if (sym == YY__LBRACK || sym == YY__LPAREN) {
		if (sym == YY__LBRACK) {
			sym = get_sym();
			save_pos  = yy_pos;
			save_text = yy_text;
			save_line = yy_line;
			alt235 = -2;
			sym2 = sym;
			if (sym2 == YY_STATIC) {
				alt235 = 236;
				goto _yy_state_235;
			} else if (YY_IN_SET(sym2, (YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\001\000\200\000\000\000\000\000")) {
				alt235 = 239;
				goto _yy_state_235;
			} else if (sym2 == YY__STAR) {
				sym2 = get_sym();
				goto _yy_state_235_7;
			} else if (YY_IN_SET(sym2, (YY__LPAREN,YY__PLUS_PLUS,YY__MINUS_MINUS,YY__AND,YY__PLUS,YY__MINUS,YY__TILDE,YY__BANG,YY_SIZEOF,YY__ALIGNOF,YY___ALIGNOF__,YY_ID,YY_OCTNUMBER,YY_DECNUMBER,YY_HEXNUMBER,YY_FLOATNUMBER,YY_STRING,YY_CHARACTER), "\000\010\000\000\000\100\200\361\377\003")) {
				alt235 = 245;
				goto _yy_state_235;
			} else if (sym2 == YY__RBRACK) {
				alt235 = 246;
				goto _yy_state_235;
			} else {
				yy_error_sym("unexpected '%s'", sym2);
			}
_yy_state_235_7:
			if (sym2 == YY__RBRACK) {
				alt235 = 244;
				goto _yy_state_235;
			} else if (YY_IN_SET(sym2, (YY__LPAREN,YY__PLUS_PLUS,YY__MINUS_MINUS,YY__AND,YY__PLUS,YY__MINUS,YY__TILDE,YY__BANG,YY_SIZEOF,YY__ALIGNOF,YY___ALIGNOF__,YY_ID,YY_OCTNUMBER,YY_DECNUMBER,YY_HEXNUMBER,YY_FLOATNUMBER,YY_STRING,YY_CHARACTER,YY__STAR), "\000\010\000\000\010\100\200\361\377\003")) {
				alt235 = 245;
				goto _yy_state_235;
			} else {
				yy_error_sym("unexpected '%s'", sym2);
			}
_yy_state_235:
			yy_pos  = save_pos;
			yy_text = save_text;
			yy_line = save_line;
			if (alt235 == 236) {
				sym = get_sym();
				if (YY_IN_SET(sym, (YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\001\000\200\000\000\000\000\000")) {
					sym = parse_type_qualifier_list(sym, &dummy);
				}
				sym = parse_assignment_expression(sym, &len);
			} else if (alt235 == 239) {
				sym = parse_type_qualifier_list(sym, &dummy);
				save_pos  = yy_pos;
				save_text = yy_text;
				save_line = yy_line;
				alt239 = -2;
				sym2 = sym;
				if (sym2 == YY_STATIC) {
					alt239 = 240;
					goto _yy_state_239;
				} else if (sym2 == YY__STAR) {
					sym2 = get_sym();
					goto _yy_state_239_2;
				} else if (YY_IN_SET(sym2, (YY__LPAREN,YY__PLUS_PLUS,YY__MINUS_MINUS,YY__AND,YY__PLUS,YY__MINUS,YY__TILDE,YY__BANG,YY_SIZEOF,YY__ALIGNOF,YY___ALIGNOF__,YY_ID,YY_OCTNUMBER,YY_DECNUMBER,YY_HEXNUMBER,YY_FLOATNUMBER,YY_STRING,YY_CHARACTER), "\000\010\000\000\000\100\200\361\377\003")) {
					alt239 = 243;
					goto _yy_state_239;
				} else if (sym2 == YY__RBRACK) {
					alt239 = 246;
					goto _yy_state_239;
				} else {
					yy_error_sym("unexpected '%s'", sym2);
				}
_yy_state_239_2:
				if (sym2 == YY__RBRACK) {
					alt239 = 242;
					goto _yy_state_239;
				} else if (YY_IN_SET(sym2, (YY__LPAREN,YY__PLUS_PLUS,YY__MINUS_MINUS,YY__AND,YY__PLUS,YY__MINUS,YY__TILDE,YY__BANG,YY_SIZEOF,YY__ALIGNOF,YY___ALIGNOF__,YY_ID,YY_OCTNUMBER,YY_DECNUMBER,YY_HEXNUMBER,YY_FLOATNUMBER,YY_STRING,YY_CHARACTER,YY__STAR), "\000\010\000\000\010\100\200\361\377\003")) {
					alt239 = 243;
					goto _yy_state_239;
				} else {
					yy_error_sym("unexpected '%s'", sym2);
				}
_yy_state_239:
				yy_pos  = save_pos;
				yy_text = save_text;
				yy_line = save_line;
				if (alt239 == 240) {
					sym = get_sym();
					sym = parse_assignment_expression(sym, &len);
				} else if (alt239 == 246) {
				} else if (alt239 == 242) {
					sym = get_sym();
				} else if (alt239 == 243) {
					sym = parse_assignment_expression(sym, &len);
				} else {
					yy_error_sym("unexpected '%s'", sym);
				}
			} else if (alt235 == 246 || alt235 == 244 || alt235 == 245) {
				if (alt235 == 246) {
				} else if (alt235 == 244) {
					sym = get_sym();
				} else if (alt235 == 245) {
					sym = parse_assignment_expression(sym, &len);
				} else {
					yy_error_sym("unexpected '%s'", sym);
				}
			} else {
				yy_error_sym("unexpected '%s'", sym);
			}
			if (sym != YY__RBRACK) {
				yy_error_sym("']' expected, got '%s'", sym);
			}
			sym = get_sym();
			sym = parse_array_or_function_declarators(sym, dcl);
			zend_ffi_make_array_type(dcl, &len);
		} else if (sym == YY__LPAREN) {
			sym = get_sym();
			if (YY_IN_SET(sym, (YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID,YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__,YY__POINT_POINT_POINT), "\000\340\377\077\302\000\000\000\010\000")) {
				if (YY_IN_SET(sym, (YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID,YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\377\077\202\000\000\000\010\000")) {
					sym = parse_parameter_declaration(sym, &args);
					while (1) {
						save_pos  = yy_pos;
						save_text = yy_text;
						save_line = yy_line;
						alt249 = -2;
						sym2 = sym;
						if (sym2 == YY__COMMA) {
							sym2 = get_sym();
							goto _yy_state_249_1;
						} else if (sym2 == YY__RPAREN) {
							alt249 = 255;
							goto _yy_state_249;
						} else {
							yy_error_sym("unexpected '%s'", sym2);
						}
_yy_state_249_1:
						if (YY_IN_SET(sym2, (YY_VOID,YY_CHAR,YY_SHORT,YY_INT,YY_LONG,YY_FLOAT,YY_DOUBLE,YY_SIGNED,YY_UNSIGNED,YY__BOOL,YY__COMPLEX,YY_STRUCT,YY_UNION,YY_ENUM,YY_ID,YY_CONST,YY_RESTRICT,YY_VOLATILE,YY__ATOMIC,YY___ATTRIBUTE__), "\000\340\377\077\202\000\000\000\010\000")) {
							alt249 = 250;
							goto _yy_state_249;
						} else if (sym2 == YY__POINT_POINT_POINT) {
							alt249 = 252;
							goto _yy_state_249;
						} else {
							yy_error_sym("unexpected '%s'", sym2);
						}
_yy_state_249:
						yy_pos  = save_pos;
						yy_text = save_text;
						yy_line = save_line;
						if (alt249 != 250) {
							break;
						}
						sym = get_sym();
						sym = parse_parameter_declaration(sym, &args);
					}
					if (alt249 == 252) {
						sym = get_sym();
						if (sym != YY__POINT_POINT_POINT) {
							yy_error_sym("'...' expected, got '%s'", sym);
						}
						sym = get_sym();
						variadic = 1;
					}
				} else if (sym == YY__POINT_POINT_POINT) {
					sym = get_sym();
				} else {
					yy_error_sym("unexpected '%s'", sym);
				}
			}
			if (sym != YY__RPAREN) {
				yy_error_sym("')' expected, got '%s'", sym);
			}
			sym = get_sym();
			sym = parse_array_or_function_declarators(sym, dcl);
			zend_ffi_make_func_type(dcl, args, variadic);
		}
	}
	return sym;
}

static int parse_parameter_declaration(int sym, HashTable **args) {
	const char *name = NULL;
	size_t name_len = 0;
	zend_ffi_dcl param_dcl = {0, 0, 0, NULL};
	sym = parse_specifier_qualifier_list(sym, &param_dcl);
	sym = parse_abstract_declarator(sym, &param_dcl, &name, &name_len);
	zend_ffi_add_arg(args, name, name_len, &param_dcl);
	return sym;
}

static int parse_type_name(int sym, zend_ffi_dcl *dcl) {
	const char *name = NULL;
	size_t name_len = 0;
	sym = parse_specifier_qualifier_list(sym, dcl);
	sym = parse_abstract_declarator(sym, dcl, &name, &name_len);
	return sym;
}

static int parse_attributes(int sym, zend_ffi_dcl *dcl) {
	if (sym != YY___ATTRIBUTE__) {
		yy_error_sym("'__attribute__' expected, got '%s'", sym);
	}
	do {
		sym = get_sym();
		if (sym != YY__LPAREN) {
			yy_error_sym("'(' expected, got '%s'", sym);
		}
		sym = get_sym();
		if (sym != YY__LPAREN) {
			yy_error_sym("'(' expected, got '%s'", sym);
		}
		sym = get_sym();
		sym = parse_attrib(sym, dcl);
		while (sym == YY__COMMA) {
			sym = get_sym();
			sym = parse_attrib(sym, dcl);
		}
		if (sym != YY__RPAREN) {
			yy_error_sym("')' expected, got '%s'", sym);
		}
		sym = get_sym();
		if (sym != YY__RPAREN) {
			yy_error_sym("')' expected, got '%s'", sym);
		}
		sym = get_sym();
	} while (sym == YY___ATTRIBUTE__);
	return sym;
}

static int parse_attrib(int sym, zend_ffi_dcl *dcl) {
	const char *name;
	size_t name_len;
	int n;
	zend_ffi_val val;
	if (sym == YY_ID) {
		sym = parse_ID(sym, &name, &name_len);
		if (sym == YY__COMMA || sym == YY__RPAREN) {
			zend_ffi_add_attribute(dcl, name, name_len);
		} else if (sym == YY__LPAREN) {
			sym = get_sym();
			sym = parse_assignment_expression(sym, &val);
			zend_ffi_add_attribute_value(dcl, name, name_len, 0, &val);
			n = 0;
			while (sym == YY__COMMA) {
				sym = get_sym();
				sym = parse_assignment_expression(sym, &val);
				zend_ffi_add_attribute_value(dcl, name, name_len, ++n, &val);
			}
			if (sym != YY__RPAREN) {
				yy_error_sym("')' expected, got '%s'", sym);
			}
			sym = get_sym();
		}
	}
	return sym;
}

static int parse_initializer(int sym) {
	int   sym2;
	const unsigned char *save_pos;
	const unsigned char *save_text;
	int   save_line;
	int alt310;
	zend_ffi_val dummy;
	if (sym != YY__EQUAL) {
		yy_error_sym("'=' expected, got '%s'", sym);
	}
	sym = get_sym();
	if (YY_IN_SET(sym, (YY__LPAREN,YY_ID,YY_OCTNUMBER,YY_DECNUMBER,YY_HEXNUMBER,YY_FLOATNUMBER,YY_STRING,YY_CHARACTER,YY__PLUS_PLUS,YY__MINUS_MINUS,YY__AND,YY__STAR,YY__PLUS,YY__MINUS,YY__TILDE,YY__BANG,YY_SIZEOF,YY__ALIGNOF,YY___ALIGNOF__), "\000\010\000\000\010\100\200\361\377\003")) {
		sym = parse_assignment_expression(sym, &dummy);
	} else if (sym == YY__LBRACE) {
		sym = get_sym();
		if (sym == YY__LBRACK || sym == YY__POINT) {
			sym = parse_designation(sym);
		}
		sym = parse_initializer(sym);
		while (1) {
			save_pos  = yy_pos;
			save_text = yy_text;
			save_line = yy_line;
			alt310 = -2;
			sym2 = sym;
			if (sym2 == YY__COMMA) {
				sym2 = get_sym();
				goto _yy_state_310_1;
			} else if (sym2 == YY__RBRACE) {
				alt310 = 315;
				goto _yy_state_310;
			} else {
				yy_error_sym("unexpected '%s'", sym2);
			}
_yy_state_310_1:
			if (sym2 == YY__LBRACK || sym2 == YY__POINT || sym2 == YY__EQUAL) {
				alt310 = 311;
				goto _yy_state_310;
			} else if (sym2 == YY__RBRACE) {
				alt310 = 314;
				goto _yy_state_310;
			} else {
				yy_error_sym("unexpected '%s'", sym2);
			}
_yy_state_310:
			yy_pos  = save_pos;
			yy_text = save_text;
			yy_line = save_line;
			if (alt310 != 311) {
				break;
			}
			sym = get_sym();
			if (sym == YY__LBRACK || sym == YY__POINT) {
				sym = parse_designation(sym);
			}
			sym = parse_initializer(sym);
		}
		if (alt310 == 314) {
			sym = get_sym();
		}
		if (sym != YY__RBRACE) {
			yy_error_sym("'}' expected, got '%s'", sym);
		}
		sym = get_sym();
	}
	return sym;
}

static int parse_designation(int sym) {
	const char *name;
	size_t name_len;
	zend_ffi_val dummy;
	do {
		if (sym == YY__LBRACK) {
			sym = get_sym();
			sym = parse_constant_expression(sym, &dummy);
			if (sym != YY__RBRACK) {
				yy_error_sym("']' expected, got '%s'", sym);
			}
			sym = get_sym();
		} else if (sym == YY__POINT) {
			sym = get_sym();
			sym = parse_ID(sym, &name, &name_len);
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	} while (sym == YY__LBRACK || sym == YY__POINT);
	if (sym != YY__EQUAL) {
		yy_error_sym("'=' expected, got '%s'", sym);
	}
	sym = get_sym();
	return sym;
}

static int parse_expr_list(int sym) {
	zend_ffi_val dummy;
	sym = parse_assignment_expression(sym, &dummy);
	while (sym == YY__COMMA) {
		sym = get_sym();
		sym = parse_assignment_expression(sym, &dummy);
	}
	return sym;
}

static int parse_expression(int sym, zend_ffi_val *val) {
	sym = parse_assignment_expression(sym, val);
	while (sym == YY__COMMA) {
		sym = get_sym();
		sym = parse_assignment_expression(sym, val);
	}
	return sym;
}

static int parse_assignment_expression(int sym, zend_ffi_val *val) {
	sym = parse_conditional_expression(sym, val);
	return sym;
}

static int parse_constant_expression(int sym, zend_ffi_val *val) {
	sym = parse_conditional_expression(sym, val);
	return sym;
}

static int parse_conditional_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2, op3;
	sym = parse_logical_or_expression(sym, val);
	if (sym == YY__QUERY) {
		sym = get_sym();
		sym = parse_expression(sym, &op2);
		if (sym != YY__COLON) {
			yy_error_sym("':' expected, got '%s'", sym);
		}
		sym = get_sym();
		sym = parse_conditional_expression(sym, &op3);
		zend_ffi_expr_conditional(val, &op2, &op3);
	}
	return sym;
}

static int parse_logical_or_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_logical_and_expression(sym, val);
	while (sym == YY__BAR_BAR) {
		sym = get_sym();
		sym = parse_logical_and_expression(sym, &op2);
		zend_ffi_expr_bool_or(val, &op2);
	}
	return sym;
}

static int parse_logical_and_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_inclusive_or_expression(sym, val);
	while (sym == YY__AND_AND) {
		sym = get_sym();
		sym = parse_inclusive_or_expression(sym, &op2);
		zend_ffi_expr_bool_and(val, &op2);
	}
	return sym;
}

static int parse_inclusive_or_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_exclusive_or_expression(sym, val);
	while (sym == YY__BAR) {
		sym = get_sym();
		sym = parse_exclusive_or_expression(sym, &op2);
		zend_ffi_expr_bw_or(val, &op2);
	}
	return sym;
}

static int parse_exclusive_or_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_and_expression(sym, val);
	while (sym == YY__UPARROW) {
		sym = get_sym();
		sym = parse_and_expression(sym, &op2);
		zend_ffi_expr_bw_xor(val, &op2);
	}
	return sym;
}

static int parse_and_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_equality_expression(sym, val);
	while (sym == YY__AND) {
		sym = get_sym();
		sym = parse_equality_expression(sym, &op2);
		zend_ffi_expr_bw_and(val, &op2);
	}
	return sym;
}

static int parse_equality_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_relational_expression(sym, val);
	while (sym == YY__EQUAL_EQUAL || sym == YY__BANG_EQUAL) {
		if (sym == YY__EQUAL_EQUAL) {
			sym = get_sym();
			sym = parse_relational_expression(sym, &op2);
			zend_ffi_expr_is_equal(val, &op2);
		} else if (sym == YY__BANG_EQUAL) {
			sym = get_sym();
			sym = parse_relational_expression(sym, &op2);
			zend_ffi_expr_is_not_equal(val, &op2);
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	}
	return sym;
}

static int parse_relational_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_shift_expression(sym, val);
	while (sym == YY__LESS || sym == YY__GREATER || sym == YY__LESS_EQUAL || sym == YY__GREATER_EQUAL) {
		if (sym == YY__LESS) {
			sym = get_sym();
			sym = parse_shift_expression(sym, &op2);
			zend_ffi_expr_is_less(val, &op2);
		} else if (sym == YY__GREATER) {
			sym = get_sym();
			sym = parse_shift_expression(sym, &op2);
			zend_ffi_expr_is_greater(val, &op2);
		} else if (sym == YY__LESS_EQUAL) {
			sym = get_sym();
			sym = parse_shift_expression(sym, &op2);
			zend_ffi_expr_is_less_or_equal(val, &op2);
		} else if (sym == YY__GREATER_EQUAL) {
			sym = get_sym();
			sym = parse_shift_expression(sym, &op2);
			zend_ffi_expr_is_greater_or_equal(val, &op2);
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	}
	return sym;
}

static int parse_shift_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_additive_expression(sym, val);
	while (sym == YY__LESS_LESS || sym == YY__GREATER_GREATER) {
		if (sym == YY__LESS_LESS) {
			sym = get_sym();
			sym = parse_additive_expression(sym, &op2);
			zend_ffi_expr_shift_left(val, &op2);
		} else if (sym == YY__GREATER_GREATER) {
			sym = get_sym();
			sym = parse_additive_expression(sym, &op2);
			zend_ffi_expr_shift_right(val, &op2);
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	}
	return sym;
}

static int parse_additive_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_multiplicative_expression(sym, val);
	while (sym == YY__PLUS || sym == YY__MINUS) {
		if (sym == YY__PLUS) {
			sym = get_sym();
			sym = parse_multiplicative_expression(sym, &op2);
			zend_ffi_expr_add(val, &op2);
		} else if (sym == YY__MINUS) {
			sym = get_sym();
			sym = parse_multiplicative_expression(sym, &op2);
			zend_ffi_expr_sub(val, &op2);
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	}
	return sym;
}

static int parse_multiplicative_expression(int sym, zend_ffi_val *val) {
	zend_ffi_val op2;
	sym = parse_cast_expression(sym, val);
	while (sym == YY__STAR || sym == YY__SLASH || sym == YY__PERCENT) {
		if (sym == YY__STAR) {
			sym = get_sym();
			sym = parse_cast_expression(sym, &op2);
			zend_ffi_expr_mul(val, &op2);
		} else if (sym == YY__SLASH) {
			sym = get_sym();
			sym = parse_cast_expression(sym, &op2);
			zend_ffi_expr_div(val, &op2);
		} else if (sym == YY__PERCENT) {
			sym = get_sym();
			sym = parse_cast_expression(sym, &op2);
			zend_ffi_expr_mod(val, &op2);
		} else {
			yy_error_sym("unexpected '%s'", sym);
		}
	}
	return sym;
}

static int parse_cast_expression(int sym, zend_ffi_val *val) {
	int do_cast = 0;
	zend_ffi_dcl dcl = {0, 0, 0, NULL};
	if (sym == YY__LPAREN) {
		sym = get_sym();
		sym = parse_type_name(sym, &dcl);
		if (sym != YY__RPAREN) {
			yy_error_sym("')' expected, got '%s'", sym);
		}
		sym = get_sym();
		do_cast = 1;
	}
	sym = parse_unary_expression(sym, val);
	if (do_cast) zend_ffi_expr_cast(val, &dcl);
	return sym;
}

static int parse_unary_expression(int sym, zend_ffi_val *val) {
	const char *name;
	size_t name_len;
	zend_ffi_dcl dcl = {0, 0, 0, NULL};
	switch (sym) {
		case YY_ID:
			sym = parse_ID(sym, &name, &name_len);
			zend_ffi_resolve_const(name, name_len, val);
			while (YY_IN_SET(sym, (YY__LBRACK,YY__LPAREN,YY__POINT,YY__MINUS_GREATER,YY__PLUS_PLUS,YY__MINUS_MINUS), "\000\010\000\000\020\001\000\070\000\000")) {
				switch (sym) {
					case YY__LBRACK:
						sym = get_sym();
						sym = parse_expr_list(sym);
						if (sym != YY__RBRACK) {
							yy_error_sym("']' expected, got '%s'", sym);
						}
						sym = get_sym();
						break;
					case YY__LPAREN:
						sym = get_sym();
						if (YY_IN_SET(sym, (YY__LPAREN,YY_ID,YY_OCTNUMBER,YY_DECNUMBER,YY_HEXNUMBER,YY_FLOATNUMBER,YY_STRING,YY_CHARACTER,YY__PLUS_PLUS,YY__MINUS_MINUS,YY__AND,YY__STAR,YY__PLUS,YY__MINUS,YY__TILDE,YY__BANG,YY_SIZEOF,YY__ALIGNOF,YY___ALIGNOF__), "\000\010\000\000\010\100\200\361\377\003")) {
							sym = parse_expr_list(sym);
						}
						if (sym != YY__RPAREN) {
							yy_error_sym("')' expected, got '%s'", sym);
						}
						sym = get_sym();
						break;
					case YY__POINT:
						sym = get_sym();
						sym = parse_ID(sym, &name, &name_len);
						break;
					case YY__MINUS_GREATER:
						sym = get_sym();
						sym = parse_ID(sym, &name, &name_len);
						break;
					case YY__PLUS_PLUS:
						sym = get_sym();
						break;
					case YY__MINUS_MINUS:
						sym = get_sym();
						break;
					default:
						yy_error_sym("unexpected '%s'", sym);
				}
				zend_ffi_val_error(val);
			}
			break;
		case YY_OCTNUMBER:
			sym = parse_OCTNUMBER(sym, val);
			break;
		case YY_DECNUMBER:
			sym = parse_DECNUMBER(sym, val);
			break;
		case YY_HEXNUMBER:
			sym = parse_HEXNUMBER(sym, val);
			break;
		case YY_FLOATNUMBER:
			sym = parse_FLOATNUMBER(sym, val);
			break;
		case YY_STRING:
			sym = parse_STRING(sym, val);
			break;
		case YY_CHARACTER:
			sym = parse_CHARACTER(sym, val);
			break;
		case YY__LPAREN:
			sym = get_sym();
			sym = parse_expression(sym, val);
			if (sym != YY__RPAREN) {
				yy_error_sym("')' expected, got '%s'", sym);
			}
			sym = get_sym();
			break;
		case YY__PLUS_PLUS:
			sym = get_sym();
			sym = parse_unary_expression(sym, val);
			zend_ffi_val_error(val);
			break;
		case YY__MINUS_MINUS:
			sym = get_sym();
			sym = parse_unary_expression(sym, val);
			zend_ffi_val_error(val);
			break;
		case YY__AND:
			sym = get_sym();
			sym = parse_cast_expression(sym, val);
			zend_ffi_val_error(val);
			break;
		case YY__STAR:
			sym = get_sym();
			sym = parse_cast_expression(sym, val);
			zend_ffi_val_error(val);
			break;
		case YY__PLUS:
			sym = get_sym();
			sym = parse_cast_expression(sym, val);
			zend_ffi_expr_plus(val);
			break;
		case YY__MINUS:
			sym = get_sym();
			sym = parse_cast_expression(sym, val);
			zend_ffi_expr_neg(val);
			break;
		case YY__TILDE:
			sym = get_sym();
			sym = parse_cast_expression(sym, val);
			zend_ffi_expr_bw_not(val);
			break;
		case YY__BANG:
			sym = get_sym();
			sym = parse_cast_expression(sym, val);
			zend_ffi_expr_bool_not(val);
			break;
		case YY_SIZEOF:
			sym = get_sym();
			if (YY_IN_SET(sym, (YY_ID,YY_OCTNUMBER,YY_DECNUMBER,YY_HEXNUMBER,YY_FLOATNUMBER,YY_STRING,YY_CHARACTER,YY__LPAREN,YY__PLUS_PLUS,YY__MINUS_MINUS,YY__AND,YY__STAR,YY__PLUS,YY__MINUS,YY__TILDE,YY__BANG,YY_SIZEOF,YY__ALIGNOF,YY___ALIGNOF__), "\000\010\000\000\010\100\200\361\377\003")) {
				sym = parse_unary_expression(sym, val);
				zend_ffi_expr_sizeof_val(val);
			} else if (sym == YY__LPAREN) {
				sym = get_sym();
				sym = parse_type_name(sym, &dcl);
				if (sym != YY__RPAREN) {
					yy_error_sym("')' expected, got '%s'", sym);
				}
				sym = get_sym();
				zend_ffi_expr_sizeof_type(val, &dcl);
			}
			break;
		case YY__ALIGNOF:
			sym = get_sym();
			if (sym != YY__LPAREN) {
				yy_error_sym("'(' expected, got '%s'", sym);
			}
			sym = get_sym();
			sym = parse_type_name(sym, &dcl);
			if (sym != YY__RPAREN) {
				yy_error_sym("')' expected, got '%s'", sym);
			}
			sym = get_sym();
			zend_ffi_expr_alignof_type(val, &dcl);
			break;
		case YY___ALIGNOF__:
			sym = get_sym();
			if (YY_IN_SET(sym, (YY_ID,YY_OCTNUMBER,YY_DECNUMBER,YY_HEXNUMBER,YY_FLOATNUMBER,YY_STRING,YY_CHARACTER,YY__LPAREN,YY__PLUS_PLUS,YY__MINUS_MINUS,YY__AND,YY__STAR,YY__PLUS,YY__MINUS,YY__TILDE,YY__BANG,YY_SIZEOF,YY__ALIGNOF,YY___ALIGNOF__), "\000\010\000\000\010\100\200\361\377\003")) {
				sym = parse_unary_expression(sym, val);
				zend_ffi_expr_alignof_val(val);
			} else if (sym == YY__LPAREN) {
				sym = get_sym();
				sym = parse_type_name(sym, &dcl);
				if (sym != YY__RPAREN) {
					yy_error_sym("')' expected, got '%s'", sym);
				}
				sym = get_sym();
				zend_ffi_expr_alignof_type(val, &dcl);
			}
			break;
	}
	return sym;
}

static int parse_ID(int sym, const char **name, size_t *name_len) {
	if (sym != YY_ID) {
		yy_error_sym("<ID> expected, got '%s'", sym);
	}
	*name = (const char*)yy_text; *name_len = yy_pos - yy_text;
	sym = get_sym();
	return sym;
}

static int parse_OCTNUMBER(int sym, zend_ffi_val *val) {
	if (sym != YY_OCTNUMBER) {
		yy_error_sym("<OCTNUMBER> expected, got '%s'", sym);
	}
	zend_ffi_val_number(val, 8, (const char*)yy_text, yy_pos - yy_text);
	sym = get_sym();
	return sym;
}

static int parse_DECNUMBER(int sym, zend_ffi_val *val) {
	if (sym != YY_DECNUMBER) {
		yy_error_sym("<DECNUMBER> expected, got '%s'", sym);
	}
	zend_ffi_val_number(val, 10, (const char*)yy_text, yy_pos - yy_text);
	sym = get_sym();
	return sym;
}

static int parse_HEXNUMBER(int sym, zend_ffi_val *val) {
	if (sym != YY_HEXNUMBER) {
		yy_error_sym("<HEXNUMBER> expected, got '%s'", sym);
	}
	zend_ffi_val_number(val, 16, (const char*)yy_text + 2, yy_pos - yy_text - 2);
	sym = get_sym();
	return sym;
}

static int parse_FLOATNUMBER(int sym, zend_ffi_val *val) {
	if (sym != YY_FLOATNUMBER) {
		yy_error_sym("<FLOATNUMBER> expected, got '%s'", sym);
	}
	zend_ffi_val_float_number(val, (const char*)yy_text, yy_pos - yy_text);
	sym = get_sym();
	return sym;
}

static int parse_STRING(int sym, zend_ffi_val *val) {
	if (sym != YY_STRING) {
		yy_error_sym("<STRING> expected, got '%s'", sym);
	}
	zend_ffi_val_string(val, (const char*)yy_text, yy_pos - yy_text);
	sym = get_sym();
	return sym;
}

static int parse_CHARACTER(int sym, zend_ffi_val *val) {
	if (sym != YY_CHARACTER) {
		yy_error_sym("<CHARACTER> expected, got '%s'", sym);
	}
	zend_ffi_val_character(val, (const char*)yy_text, yy_pos - yy_text);
	sym = get_sym();
	return sym;
}

static void parse(void) {
	int sym;

	yy_pos = yy_text = yy_buf;
	yy_line = 1;
	sym = parse_declarations(get_sym());
	if (sym != YY_EOF) {
		yy_error_sym("<EOF> expected, got '%s'", sym);
	}
}

int zend_ffi_parse_decl(zend_string *str) {
	FFI_G(error) = NULL;
	yy_buf = (unsigned char*)ZSTR_VAL(str);
	yy_end = yy_buf + ZSTR_LEN(str);
	parse();
	return FFI_G(error) ? FAILURE : SUCCESS;
}

int zend_ffi_parse_type(zend_string *str, zend_ffi_dcl *dcl) {
	int sym;

	FFI_G(error) = NULL;
	yy_pos = yy_text = yy_buf = (unsigned char*)ZSTR_VAL(str);
	yy_end = yy_buf + ZSTR_LEN(str);
	yy_line = 1;
	sym = parse_type_name(get_sym(), dcl);
	if (sym != YY_EOF) {
		yy_error_sym("<EOF> expected, got '%s'", sym);
	}
	return FFI_G(error) ? FAILURE : SUCCESS;
}

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * End:
 */
