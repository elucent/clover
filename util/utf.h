#ifndef UTILITY_UTF_H
#define UTILITY_UTF_H

#include "rt/def.h"
#include "util/io.h"

static constexpr const uint8_t UTF8_FOUR = 0b11110000,
            UTF8_THREE = 0b11100000,
            UTF8_TWO = 0b11000000,
            UTF8_INNER = 0b10000000;

static constexpr const uint8_t UTF8_ONE_MASK = 0b01111111,
            UTF8_INNER_MASK = 0b00111111,
            UTF8_TWO_MASK = 0b00011111,
            UTF8_THREE_MASK = 0b00001111,
            UTF8_FOUR_MASK = 0b00000111;

static constexpr const uint32_t UTF8_ONE_MAX = 0x7f,
            UTF8_TWO_MAX = 0x7ff,
            UTF8_THREE_MAX = 0xffff,
            UTF8_FOUR_MAX = 0x1fffff;

// representation of a single unicode character
struct rune {
    uint32_t u;

    inline constexpr rune(): u(0) {}
    inline constexpr rune(uint32_t u_in): u(u_in) {}
    inline constexpr u32 get() const { return u; }
    inline constexpr u32 ref() { return u; }

    inline constexpr u32 bytes() const {
        if (u <= UTF8_ONE_MAX) return 1;
        else if (u <= UTF8_TWO_MAX) return 2;
        else if (u <= UTF8_THREE_MAX) return 3;
        else return 4;
    }

    inline constexpr bool operator==(const rune& other) const {
        return get() == other.get();
    }

    inline constexpr bool operator!=(const rune& other) const {
        return get() != other.get();
    }
};

typedef enum UnicodeCategory_t {
    UNICODE_INVALID = 0,
    UNICODE_CONTROL = 1,
    UNICODE_FORMAT = 2,
    UNICODE_NOT_ASSIGNED = 3,
    UNICODE_PRIVATE_USE = 4,
    UNICODE_SURROGATE = 5,
    UNICODE_CASED_LETTER = 6,
    UNICODE_LOWERCASE_LETTER = 7,
    UNICODE_LETTER_MODIFIER = 8,
    UNICODE_OTHER_LETTER = 9,
    UNICODE_TITLECASE_LETTER = 10,
    UNICODE_UPPERCASE_LETTER = 11,
    UNICODE_SPACING_COMBINING_MARK = 12,
    UNICODE_ENCLOSING_MARK = 13,
    UNICODE_NONSPACING_MARK = 14,
    UNICODE_DECIMAL_NUMBER = 15,
    UNICODE_LETTER_NUMBER = 16,
    UNICODE_OTHER_NUMBER = 17,
    UNICODE_PUNCTUATION_CONNECTOR = 18,
    UNICODE_PUNCTUATION_DASH = 19,
    UNICODE_PUNCTUATION_CLOSE = 20,
    UNICODE_PUNCTUATION_FINAL_QUOTE = 21,
    UNICODE_PUNCTUATION_INITIAL_QUOTE = 22,
    UNICODE_PUNCTUATION_OTHER = 23,
    UNICODE_PUNCTUATION_OPEN = 24,
    UNICODE_CURRENCY_SYMBOL = 25,
    UNICODE_MODIFIER_SYMBOL = 26,
    UNICODE_MATH_SYMBOL = 27,
    UNICODE_OTHER_SYMBOL = 28,
    UNICODE_LINE_SEPARATOR = 29,
    UNICODE_PARAGRAPH_SEPARATOR = 30,
    UNICODE_SPACE_SEPARATOR = 31
} UnicodeCategory;

typedef enum UnicodeError_t {
    NO_ERROR = 0,
    INCORRECT_FORMAT,
    RAN_OUT_OF_BOUNDS,
    BUFFER_TOO_SMALL,
    INVALID_RUNE
} UnicodeError;

struct amounts {
    unsigned long int runes, bytes;
};

// returns the most recent internal error, or NO_ERROR if there was none.
UnicodeError unicode_error();

// moves a pointer forward by one rune
const char* utf8_forward(const char* str);

// moves a pointer backward by one rune
const char* utf8_backward(const char* str);

// moves a pointer forward by one rune, writing the rune to out
const char* utf8_decode_forward(const char* str, rune* out);

// moves a pointer backward by one rune, writing the rune to out
const char* utf8_decode_backward(const char* str, rune* out);

// returns the rune at the head of the provided string
rune utf8_rune_at(const char* str);

// returns the length of str in runes
unsigned long int utf8_length(const char* str, unsigned long int str_length);

// returns the length of str in bytes
unsigned long int utf8_bytes(const rune* str, unsigned long int str_length);

// decodes str_length bytes from str into runes, and writes up to out_length runes to out.
// returns the number of bytes and runes written to out.
amounts utf8_decode(const char* str, unsigned long int str_length, 
                              rune* out, unsigned long int out_length);

// encodes str_length runes from str, writing up to out_length bytes to out.
// returns the number of bytes written to out.
unsigned long int utf8_encode(const rune* str, unsigned long int str_length,
                              char* out, unsigned long int out_length);

// lexicographically compares strings a and b
// returns comparison result
long int utf8_compare(const char* a, unsigned long int a_length,
                      const char* b, unsigned long int b_length);

// these are todo for now. probably not too hard, right? :-)
unsigned long int utf16_length(const char* str, unsigned long int str_length);
unsigned long int utf16_decode(const char* str, unsigned long int str_length, 
                               rune* out, unsigned long int out_length);
unsigned long int utf16_encode(const rune* str, unsigned long int str_length,
                               char* out, unsigned long int out_length);

template<typename IO, typename Format = Formatter<IO>>
inline IO format_impl(IO io, rune c) {
    slice<i8> buf = Format::reserve(io, 4);
    return Format::advance(io, utf8_encode(&c, 1, buf.data(), 4));
}

#ifndef UTF8_MINIMAL

// returns the decimal digit value of the provided rune. sets error if rune is not a digit
int utf8_digit_value(rune r);

int utf8_is_other(rune r);
int utf8_is_control(rune r);
int utf8_is_format(rune r);
int utf8_is_not_assigned(rune r);
int utf8_is_private_use(rune r);
int utf8_is_surrogate(rune r);

int utf8_is_letter(rune r);
int utf8_is_cased_letter(rune r);
int utf8_is_lowercase(rune r);
int utf8_is_letter_modifier(rune r);
int utf8_is_other_letter(rune r);
int utf8_is_titlecase(rune r);
int utf8_is_uppercase(rune r);

int utf8_is_mark(rune r);
int utf8_is_spacing_combining_mark(rune r);
int utf8_is_enclosing_mark(rune r);
int utf8_is_nonspacing_mark(rune r);

int utf8_is_number(rune r);
int utf8_is_digit(rune r);
int utf8_is_letter_number(rune r);
int utf8_is_other_number(rune r);

int utf8_is_punctuation(rune r);
int utf8_is_connector(rune r);
int utf8_is_dash(rune r);
int utf8_is_punctuation_close(rune r);
int utf8_is_final_quote(rune r);
int utf8_is_initial_quote(rune r);
int utf8_is_other_punctuation(rune r);
int utf8_is_punctuation_open(rune r);

int utf8_is_symbol(rune r);
int utf8_is_currency_symbol(rune r);
int utf8_is_modifier_symbol(rune r);
int utf8_is_math_symbol(rune r);
int utf8_is_other_symbol(rune r);

int utf8_is_separator(rune r);
int utf8_is_line_separator(rune r);
int utf8_is_paragraph_separator(rune r);
int utf8_is_space_separator(rune r);

#ifdef INCLUDE_UTF8_LOOKUP_TABLE
UnicodeCategory utf8_category(rune r);
#endif

#endif

#endif