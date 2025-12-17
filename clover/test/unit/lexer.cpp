#include "clover/test/unit/helpers.h"

TEST(lex_identifiers_ascii_letters_lowercase) {
    auto artifact = LEX("a bc def ghij kl mnopq rst uvwxy z");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("a"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("bc"), Pos(0, 2)));
    ASSERT(tokens[2] == Token(SYM("def"), Pos(0, 5)));
    ASSERT(tokens[3] == Token(SYM("ghij"), Pos(0, 9)));
    ASSERT(tokens[4] == Token(SYM("kl"), Pos(0, 14)));
    ASSERT(tokens[5] == Token(SYM("mnopq"), Pos(0, 17)));
    ASSERT(tokens[6] == Token(SYM("rst"), Pos(0, 23)));
    ASSERT(tokens[7] == Token(SYM("uvwxy"), Pos(0, 27)));
    ASSERT(tokens[8] == Token(SYM("z"), Pos(0, 33)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_identifiers_ascii_letters_uppercase) {
    auto artifact = LEX("ABCD E  F GHI JKL M NOPQR STUV WXYZ");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("ABCD"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("E"), Pos(0, 5)));
    ASSERT(tokens[2] == Token(SYM("F"), Pos(0, 8)));
    ASSERT(tokens[3] == Token(SYM("GHI"), Pos(0, 10)));
    ASSERT(tokens[4] == Token(SYM("JKL"), Pos(0, 14)));
    ASSERT(tokens[5] == Token(SYM("M"), Pos(0, 18)));
    ASSERT(tokens[6] == Token(SYM("NOPQR"), Pos(0, 20)));
    ASSERT(tokens[7] == Token(SYM("STUV"), Pos(0, 26)));
    ASSERT(tokens[8] == Token(SYM("WXYZ"), Pos(0, 31)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_identifiers_ascii_letters_mixed_case) {
    auto artifact = LEX("Abcd eFgHi JKl mNOPQ rSTuV w X Yz");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("Abcd"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("eFgHi"), Pos(0, 5)));
    ASSERT(tokens[2] == Token(SYM("JKl"), Pos(0, 11)));
    ASSERT(tokens[3] == Token(SYM("mNOPQ"), Pos(0, 15)));
    ASSERT(tokens[4] == Token(SYM("rSTuV"), Pos(0, 21)));
    ASSERT(tokens[5] == Token(SYM("w"), Pos(0, 27)));
    ASSERT(tokens[6] == Token(SYM("X"), Pos(0, 29)));
    ASSERT(tokens[7] == Token(SYM("Yz"), Pos(0, 31)));
    ASSERT(tokens.size() == 8);
}

TEST(lex_identifiers_letters_and_digits) {
    auto artifact = LEX("abc1 a234 A9b8 a01003C");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("abc1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("a234"), Pos(0, 5)));
    ASSERT(tokens[2] == Token(SYM("A9b8"), Pos(0, 10)));
    ASSERT(tokens[3] == Token(SYM("a01003C"), Pos(0, 15)));
    ASSERT(tokens.size() == 4);
}

TEST(lex_identifiers_underscore) {
    auto artifact = LEX("_ _abc _m0 __ __a__ a_b a_0 _42 _1_");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("_"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("_abc"), Pos(0, 2)));
    ASSERT(tokens[2] == Token(SYM("_m0"), Pos(0, 7)));
    ASSERT(tokens[3] == Token(SYM("__"), Pos(0, 11)));
    ASSERT(tokens[4] == Token(SYM("__a__"), Pos(0, 14)));
    ASSERT(tokens[5] == Token(SYM("a_b"), Pos(0, 20)));
    ASSERT(tokens[6] == Token(SYM("a_0"), Pos(0, 24)));
    ASSERT(tokens[7] == Token(SYM("_42"), Pos(0, 28)));
    ASSERT(tokens[8] == Token(SYM("_1_"), Pos(0, 32)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_identifiers_with_suffix_punctuators) {
    auto artifact = LEX("even? odd? set! clear! what?! _!!??");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("even?"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("odd?"), Pos(0, 6)));
    ASSERT(tokens[2] == Token(SYM("set!"), Pos(0, 11)));
    ASSERT(tokens[3] == Token(SYM("clear!"), Pos(0, 16)));
    ASSERT(tokens[4] == Token(SYM("what?!"), Pos(0, 23)));
    ASSERT(tokens[5] == Token(SYM("_!!??"), Pos(0, 30)));
    ASSERT(tokens.size() == 6);
}

TEST(lex_identifiers_unicode_lowercase_letter_start) {
    auto artifact = LEX("ña კატა 𝐯𝐚𝐫");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("ña"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("კატა"), Pos(0, 3)));
    ASSERT(tokens[2] == Token(SYM("𝐯𝐚𝐫"), Pos(0, 8)));
    ASSERT(tokens.size() == 3);
}

TEST(lex_identifiers_unicode_modifier_letter_start) {
    auto artifact = LEX("ʰ ᴮᵃᵗ 𖾓a");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("ʰ"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("ᴮᵃᵗ"), Pos(0, 2)));
    ASSERT(tokens[2] == Token(SYM("𖾓a"), Pos(0, 6)));
    ASSERT(tokens.size() == 3);
}

TEST(lex_identifiers_unicode_other_letter_start) {
    auto artifact = LEX("تت कa 𐚱");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("تت"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("कa"), Pos(0, 3)));
    ASSERT(tokens[2] == Token(SYM("𐚱"), Pos(0, 6)));
    ASSERT(tokens.size() == 3);
}

TEST(lex_identifiers_unicode_titlecase_letter_start) {
    auto artifact = LEX("ǲ_0 ῌ0a");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("ǲ_0"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("ῌ0a"), Pos(0, 4)));
    ASSERT(tokens.size() == 2);
}

TEST(lex_identifiers_unicode_uppercase_letter_start) {
    auto artifact = LEX("Ĝr ℒa 𐲨");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("Ĝr"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("ℒa"), Pos(0, 3)));
    ASSERT(tokens[2] == Token(SYM("𐲨"), Pos(0, 6)));
    ASSERT(tokens.size() == 3);
}

TEST(lex_identifiers_unicode_letter_number_start) {
    auto artifact = LEX("ᛯ ↈxy 𒐋_0");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("ᛯ"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("ↈxy"), Pos(0, 2)));
    ASSERT(tokens[2] == Token(SYM("𒐋_0"), Pos(0, 6)));
    ASSERT(tokens.size() == 3);
}

TEST(lex_identifiers_unicode_digit) {
    auto artifact = LEX("x0 y٤ z᭕ w𝟰");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("x0"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("y٤"), Pos(0, 3)));
    ASSERT(tokens[2] == Token(SYM("z᭕"), Pos(0, 6)));
    ASSERT(tokens[3] == Token(SYM("w𝟰"), Pos(0, 9)));
    ASSERT(tokens.size() == 4);
}

TEST(lex_identifiers_unicode_nonspacing_mark) {
    auto artifact = LEX("xá ya⃰ za𑨾");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("xá"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("ya⃰"), Pos(0, 4)));
    ASSERT(tokens[2] == Token(SYM("za𑨾"), Pos(0, 8)));
    ASSERT(tokens.size() == 3);
}

TEST(lex_identifiers_unicode_connector_punctuation) {
    auto artifact = LEX("x‿y z＿z");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("x‿y"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("z＿z"), Pos(0, 4)));
    ASSERT(tokens.size() == 2);
}

TEST(lex_identifiers_and_integers) {
    auto artifact = LEX("x2 2 2x _2 2_ 3_3");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("x2"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 5)));
    ASSERT(tokens[3] == Token(SYM("x"), Pos(0, 6)));
    ASSERT(tokens[4] == Token(SYM("_2"), Pos(0, 8)));
    ASSERT(tokens[5] == Token(SYM("2"), Pos(0, 11)));
    ASSERT(tokens[6] == Token(SYM("_"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(SYM("3"), Pos(0, 14)));
    ASSERT(tokens[8] == Token(SYM("_3"), Pos(0, 15)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_identifiers_and_decimals) {
    auto artifact = LEX("2.0 2.x x2. x.2 0.2x 2x.2 0.2 2.2_ 2._2 _2_._2");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("2.0"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(SYM("2"), Pos(0, 4)));
    ASSERT(tokens[2] == Token(PunctuatorDot, Pos(0, 5)));
    ASSERT(tokens[3] == Token(SYM("x"), Pos(0, 6)));
    ASSERT(tokens[4] == Token(SYM("x2"), Pos(0, 8)));
    ASSERT(tokens[5] == Token(PunctuatorDot, Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(PunctuatorDot, Pos(0, 13)));
    ASSERT(tokens[8] == Token(SYM("2"), Pos(0, 14)));
    ASSERT(tokens[9] == Token(SYM("0.2"), Pos(0, 16)));
    ASSERT(tokens[10] == Token(SYM("x"), Pos(0, 19)));
    ASSERT(tokens[11] == Token(SYM("2"), Pos(0, 21)));
    ASSERT(tokens[12] == Token(SYM("x"), Pos(0, 22)));
    ASSERT(tokens[13] == Token(PunctuatorDot, Pos(0, 23)));
    ASSERT(tokens[14] == Token(SYM("2"), Pos(0, 24)));
    ASSERT(tokens[15] == Token(SYM("0.2"), Pos(0, 26)));
    ASSERT(tokens[16] == Token(SYM("2.2"), Pos(0, 30)));
    ASSERT(tokens[17] == Token(SYM("_"), Pos(0, 33)));
    ASSERT(tokens[18] == Token(SYM("2"), Pos(0, 35)));
    ASSERT(tokens[19] == Token(PunctuatorDot, Pos(0, 36)));
    ASSERT(tokens[20] == Token(SYM("_2"), Pos(0, 37)));
    ASSERT(tokens[21] == Token(SYM("_2_"), Pos(0, 40)));
    ASSERT(tokens[22] == Token(PunctuatorDot, Pos(0, 43)));
    ASSERT(tokens[23] == Token(SYM("_2"), Pos(0, 44)));
    ASSERT(tokens.size() == 24);
}

TEST(lex_single_delimiters) {
    auto artifact = LEX("(){}[]:;.,");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(PunctuatorLeftParen, Pos(0, 0)));
    ASSERT(tokens[1] == Token(PunctuatorRightParen, Pos(0, 1)));
    ASSERT(tokens[2] == Token(PunctuatorLeftBrace, Pos(0, 2)));
    ASSERT(tokens[3] == Token(PunctuatorRightBrace, Pos(0, 3)));
    ASSERT(tokens[4] == Token(PunctuatorLeftBracket, Pos(0, 4)));
    ASSERT(tokens[5] == Token(PunctuatorRightBracket, Pos(0, 5)));
    ASSERT(tokens[6] == Token(PunctuatorColon, Pos(0, 6)));
    ASSERT(tokens[7] == Token(PunctuatorSemicolon, Pos(0, 7)));
    ASSERT(tokens[8] == Token(PunctuatorDot, Pos(0, 8)));
    ASSERT(tokens[9] == Token(PunctuatorComma, Pos(0, 9)));
    ASSERT(tokens.size() == 10);
}

TEST(lex_delimiters_separate_identifiers) {
    auto artifact = LEX("a(b)c{d}e[f]g:h;i.j,k l\tm  n");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("a"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(PunctuatorLeftParen, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("b"), Pos(0, 2)));
    ASSERT(tokens[3] == Token(PunctuatorRightParen, Pos(0, 3)));
    ASSERT(tokens[4] == Token(SYM("c"), Pos(0, 4)));
    ASSERT(tokens[5] == Token(PunctuatorLeftBrace, Pos(0, 5)));
    ASSERT(tokens[6] == Token(SYM("d"), Pos(0, 6)));
    ASSERT(tokens[7] == Token(PunctuatorRightBrace, Pos(0, 7)));
    ASSERT(tokens[8] == Token(SYM("e"), Pos(0, 8)));
    ASSERT(tokens[9] == Token(PunctuatorLeftBracket, Pos(0, 9)));
    ASSERT(tokens[10] == Token(SYM("f"), Pos(0, 10)));
    ASSERT(tokens[11] == Token(PunctuatorRightBracket, Pos(0, 11)));
    ASSERT(tokens[12] == Token(SYM("g"), Pos(0, 12)));
    ASSERT(tokens[13] == Token(PunctuatorColon, Pos(0, 13)));
    ASSERT(tokens[14] == Token(SYM("h"), Pos(0, 14)));
    ASSERT(tokens[15] == Token(PunctuatorSemicolon, Pos(0, 15)));
    ASSERT(tokens[16] == Token(SYM("i"), Pos(0, 16)));
    ASSERT(tokens[17] == Token(PunctuatorDot, Pos(0, 17)));
    ASSERT(tokens[18] == Token(SYM("j"), Pos(0, 18)));
    ASSERT(tokens[19] == Token(PunctuatorComma, Pos(0, 19)));
    ASSERT(tokens[20] == Token(SYM("k"), Pos(0, 20)));
    ASSERT(tokens[21] == Token(SYM("l"), Pos(0, 22)));
    ASSERT(tokens[22] == Token(SYM("m"), Pos(0, 24)));
    ASSERT(tokens[23] == Token(SYM("n"), Pos(0, 27)));
    ASSERT(tokens.size() == 24);
}

TEST(lex_operator_add) {
    auto artifact = LEX("1+2 1.0+2.0 x+_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorAdd, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 2)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 4)));
    ASSERT(tokens[4] == Token(OperatorAdd, Pos(0, 7)));
    ASSERT(tokens[5] == Token(SYM("2.0"), Pos(0, 8)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorAdd, Pos(0, 13)));
    ASSERT(tokens[8] == Token(SYM("_y"), Pos(0, 14)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_operator_add_assign) {
    auto artifact = LEX("1+=2 1.0+=+2.0 x+=_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorAddAssign, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 5)));
    ASSERT(tokens[4] == Token(OperatorAddAssign, Pos(0, 8)));
    ASSERT(tokens[5] == Token(OperatorAdd, Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("2.0"), Pos(0, 11)));
    ASSERT(tokens[7] == Token(SYM("x"), Pos(0, 15)));
    ASSERT(tokens[8] == Token(OperatorAddAssign, Pos(0, 16)));
    ASSERT(tokens[9] == Token(SYM("_y"), Pos(0, 18)));
    ASSERT(tokens.size() == 10);
}

TEST(lex_operator_sub) {
    auto artifact = LEX("1-2 1.0-2.0 x-_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorSub, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 2)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 4)));
    ASSERT(tokens[4] == Token(OperatorSub, Pos(0, 7)));
    ASSERT(tokens[5] == Token(SYM("2.0"), Pos(0, 8)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorSub, Pos(0, 13)));
    ASSERT(tokens[8] == Token(SYM("_y"), Pos(0, 14)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_operator_sub_assign) {
    auto artifact = LEX("1-=2 1.0-=-2.0 x-=_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorSubAssign, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 5)));
    ASSERT(tokens[4] == Token(OperatorSubAssign, Pos(0, 8)));
    ASSERT(tokens[5] == Token(OperatorSub, Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("2.0"), Pos(0, 11)));
    ASSERT(tokens[7] == Token(SYM("x"), Pos(0, 15)));
    ASSERT(tokens[8] == Token(OperatorSubAssign, Pos(0, 16)));
    ASSERT(tokens[9] == Token(SYM("_y"), Pos(0, 18)));
    ASSERT(tokens.size() == 10);
}

TEST(lex_operator_incr) {
    auto artifact = LEX("1++ ++1 x++ +++y+ ++z");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorIncr, Pos(0, 1)));
    ASSERT(tokens[2] == Token(OperatorIncr, Pos(0, 4)));
    ASSERT(tokens[3] == Token(SYM("1"), Pos(0, 6)));
    ASSERT(tokens[4] == Token(SYM("x"), Pos(0, 8)));
    ASSERT(tokens[5] == Token(OperatorIncr, Pos(0, 9)));
    ASSERT(tokens[6] == Token(OperatorIncr, Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorAdd, Pos(0, 14)));
    ASSERT(tokens[8] == Token(SYM("y"), Pos(0, 15)));
    ASSERT(tokens[9] == Token(OperatorAdd, Pos(0, 16)));
    ASSERT(tokens[10] == Token(OperatorIncr, Pos(0, 18)));
    ASSERT(tokens[11] == Token(SYM("z"), Pos(0, 20)));
    ASSERT(tokens.size() == 12);
}

TEST(lex_operator_dec) {
    auto artifact = LEX("1-- --1 x-- ---y- --z");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorDecr, Pos(0, 1)));
    ASSERT(tokens[2] == Token(OperatorDecr, Pos(0, 4)));
    ASSERT(tokens[3] == Token(SYM("1"), Pos(0, 6)));
    ASSERT(tokens[4] == Token(SYM("x"), Pos(0, 8)));
    ASSERT(tokens[5] == Token(OperatorDecr, Pos(0, 9)));
    ASSERT(tokens[6] == Token(OperatorDecr, Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorSub, Pos(0, 14)));
    ASSERT(tokens[8] == Token(SYM("y"), Pos(0, 15)));
    ASSERT(tokens[9] == Token(OperatorSub, Pos(0, 16)));
    ASSERT(tokens[10] == Token(OperatorDecr, Pos(0, 18)));
    ASSERT(tokens[11] == Token(SYM("z"), Pos(0, 20)));
    ASSERT(tokens.size() == 12);
}

TEST(lex_operator_mul) {
    auto artifact = LEX("1*2 1 * 2 x *yy* *z");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorMul, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 2)));
    ASSERT(tokens[3] == Token(SYM("1"), Pos(0, 4)));
    ASSERT(tokens[4] == Token(OperatorMul, Pos(0, 6)));
    ASSERT(tokens[5] == Token(SYM("2"), Pos(0, 8)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 10)));
    ASSERT(tokens[7] == Token(OperatorMul, Pos(0, 12)));
    ASSERT(tokens[8] == Token(SYM("yy"), Pos(0, 13)));
    ASSERT(tokens[9] == Token(OperatorMul, Pos(0, 15)));
    ASSERT(tokens[10] == Token(OperatorMul, Pos(0, 17)));
    ASSERT(tokens[11] == Token(SYM("z"), Pos(0, 18)));
    ASSERT(tokens.size() == 12);
}

TEST(lex_operator_mul_assign) {
    auto artifact = LEX("1*=2 1 *= 2 x *=yy*= *z");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorMulAssign, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[3] == Token(SYM("1"), Pos(0, 5)));
    ASSERT(tokens[4] == Token(OperatorMulAssign, Pos(0, 7)));
    ASSERT(tokens[5] == Token(SYM("2"), Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorMulAssign, Pos(0, 14)));
    ASSERT(tokens[8] == Token(SYM("yy"), Pos(0, 16)));
    ASSERT(tokens[9] == Token(OperatorMulAssign, Pos(0, 18)));
    ASSERT(tokens[10] == Token(OperatorMul, Pos(0, 21)));
    ASSERT(tokens[11] == Token(SYM("z"), Pos(0, 22)));
    ASSERT(tokens.size() == 12);
}

TEST(lex_operator_exp) {
    auto artifact = LEX("1**2 1 ** 2 x* **yy** *z");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorExp, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[3] == Token(SYM("1"), Pos(0, 5)));
    ASSERT(tokens[4] == Token(OperatorExp, Pos(0, 7)));
    ASSERT(tokens[5] == Token(SYM("2"), Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorMul, Pos(0, 13)));
    ASSERT(tokens[8] == Token(OperatorExp, Pos(0, 15)));
    ASSERT(tokens[9] == Token(SYM("yy"), Pos(0, 17)));
    ASSERT(tokens[10] == Token(OperatorExp, Pos(0, 19)));
    ASSERT(tokens[11] == Token(OperatorMul, Pos(0, 22)));
    ASSERT(tokens[12] == Token(SYM("z"), Pos(0, 23)));
    ASSERT(tokens.size() == 13);
}

TEST(lex_operator_exp_assign) {
    auto artifact = LEX("1**=2 1 **= 2 x* ** =yy**= *z");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorExpAssign, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 4)));
    ASSERT(tokens[3] == Token(SYM("1"), Pos(0, 6)));
    ASSERT(tokens[4] == Token(OperatorExpAssign, Pos(0, 8)));
    ASSERT(tokens[5] == Token(SYM("2"), Pos(0, 12)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 14)));
    ASSERT(tokens[7] == Token(OperatorMul, Pos(0, 15)));
    ASSERT(tokens[8] == Token(OperatorExp, Pos(0, 17)));
    ASSERT(tokens[9] == Token(OperatorAssign, Pos(0, 20)));
    ASSERT(tokens[10] == Token(SYM("yy"), Pos(0, 21)));
    ASSERT(tokens[11] == Token(OperatorExpAssign, Pos(0, 23)));
    ASSERT(tokens[12] == Token(OperatorMul, Pos(0, 27)));
    ASSERT(tokens[13] == Token(SYM("z"), Pos(0, 28)));
    ASSERT(tokens.size() == 14);
}

TEST(lex_operator_div) {
    auto artifact = LEX("1/2 1.0/2.0 x/_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorDiv, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 2)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 4)));
    ASSERT(tokens[4] == Token(OperatorDiv, Pos(0, 7)));
    ASSERT(tokens[5] == Token(SYM("2.0"), Pos(0, 8)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorDiv, Pos(0, 13)));
    ASSERT(tokens[8] == Token(SYM("_y"), Pos(0, 14)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_operator_div_assign) {
    auto artifact = LEX("1/=2 1.0/=/2.0 x/=_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorDivAssign, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 5)));
    ASSERT(tokens[4] == Token(OperatorDivAssign, Pos(0, 8)));
    ASSERT(tokens[5] == Token(OperatorDiv, Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("2.0"), Pos(0, 11)));
    ASSERT(tokens[7] == Token(SYM("x"), Pos(0, 15)));
    ASSERT(tokens[8] == Token(OperatorDivAssign, Pos(0, 16)));
    ASSERT(tokens[9] == Token(SYM("_y"), Pos(0, 18)));
    ASSERT(tokens.size() == 10);
}

TEST(lex_operator_rem) {
    auto artifact = LEX("1%2 1.0%2.0 x%_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorRem, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 2)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 4)));
    ASSERT(tokens[4] == Token(OperatorRem, Pos(0, 7)));
    ASSERT(tokens[5] == Token(SYM("2.0"), Pos(0, 8)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorRem, Pos(0, 13)));
    ASSERT(tokens[8] == Token(SYM("_y"), Pos(0, 14)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_operator_rem_assign) {
    auto artifact = LEX("1%=2 1.0%=%2.0 x%=_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorRemAssign, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 5)));
    ASSERT(tokens[4] == Token(OperatorRemAssign, Pos(0, 8)));
    ASSERT(tokens[5] == Token(OperatorRem, Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("2.0"), Pos(0, 11)));
    ASSERT(tokens[7] == Token(SYM("x"), Pos(0, 15)));
    ASSERT(tokens[8] == Token(OperatorRemAssign, Pos(0, 16)));;
    ASSERT(tokens[9] == Token(SYM("_y"), Pos(0, 18)));
    ASSERT(tokens.size() == 10);
}

TEST(lex_operator_bitand) {
    auto artifact = LEX("1&2 1.0&2.0 x&_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorBitAnd, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 2)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 4)));
    ASSERT(tokens[4] == Token(OperatorBitAnd, Pos(0, 7)));
    ASSERT(tokens[5] == Token(SYM("2.0"), Pos(0, 8)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorBitAnd, Pos(0, 13)));
    ASSERT(tokens[8] == Token(SYM("_y"), Pos(0, 14)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_operator_bitand_assign) {
    auto artifact = LEX("1&=2 1.0&=&2.0 x&=_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorBitAndAssign, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 5)));
    ASSERT(tokens[4] == Token(OperatorBitAndAssign, Pos(0, 8)));
    ASSERT(tokens[5] == Token(OperatorBitAnd, Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("2.0"), Pos(0, 11)));
    ASSERT(tokens[7] == Token(SYM("x"), Pos(0, 15)));
    ASSERT(tokens[8] == Token(OperatorBitAndAssign, Pos(0, 16)));
    ASSERT(tokens[9] == Token(SYM("_y"), Pos(0, 18)));
    ASSERT(tokens.size() == 10);
}

TEST(lex_operator_bitor) {
    auto artifact = LEX("1|2 1.0|2.0 x|_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorBitOr, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 2)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 4)));
    ASSERT(tokens[4] == Token(OperatorBitOr, Pos(0, 7)));
    ASSERT(tokens[5] == Token(SYM("2.0"), Pos(0, 8)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorBitOr, Pos(0, 13)));
    ASSERT(tokens[8] == Token(SYM("_y"), Pos(0, 14)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_operator_bitor_assign) {
    auto artifact = LEX("1|=2 1.0|=|2.0 x|=_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorBitOrAssign, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 5)));
    ASSERT(tokens[4] == Token(OperatorBitOrAssign, Pos(0, 8)));
    ASSERT(tokens[5] == Token(OperatorBitOr, Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("2.0"), Pos(0, 11)));
    ASSERT(tokens[7] == Token(SYM("x"), Pos(0, 15)));
    ASSERT(tokens[8] == Token(OperatorBitOrAssign, Pos(0, 16)));
    ASSERT(tokens[9] == Token(SYM("_y"), Pos(0, 18)));
    ASSERT(tokens.size() == 10);
}

TEST(lex_operator_bitxor) {
    auto artifact = LEX("1^2 1.0^2.0 x^_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorBitXor, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 2)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 4)));
    ASSERT(tokens[4] == Token(OperatorBitXor, Pos(0, 7)));
    ASSERT(tokens[5] == Token(SYM("2.0"), Pos(0, 8)));
    ASSERT(tokens[6] == Token(SYM("x"), Pos(0, 12)));
    ASSERT(tokens[7] == Token(OperatorBitXor, Pos(0, 13)));
    ASSERT(tokens[8] == Token(SYM("_y"), Pos(0, 14)));
    ASSERT(tokens.size() == 9);
}

TEST(lex_operator_bitxor_assign) {
    auto artifact = LEX("1^=2 1.0^=^2.0 x^=_y");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("1"), Pos(0, 0)));
    ASSERT(tokens[1] == Token(OperatorBitXorAssign, Pos(0, 1)));
    ASSERT(tokens[2] == Token(SYM("2"), Pos(0, 3)));
    ASSERT(tokens[3] == Token(SYM("1.0"), Pos(0, 5)));
    ASSERT(tokens[4] == Token(OperatorBitXorAssign, Pos(0, 8)));
    ASSERT(tokens[5] == Token(OperatorBitXor, Pos(0, 10)));
    ASSERT(tokens[6] == Token(SYM("2.0"), Pos(0, 11)));
    ASSERT(tokens[7] == Token(SYM("x"), Pos(0, 15)));
    ASSERT(tokens[8] == Token(OperatorBitXorAssign, Pos(0, 16)));
    ASSERT(tokens[9] == Token(SYM("_y"), Pos(0, 18)));
    ASSERT(tokens.size() == 10);
}

TEST(lex_newlines_simple) {
    auto artifact = LEX(R"(
abc
def

ghi
)");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("abc"), Pos(1, 0)));
    ASSERT(tokens[1] == Token(WhitespaceNewline, Pos(1, 3)));
    ASSERT(tokens[2] == Token(SYM("def"), Pos(2, 0)));
    ASSERT(tokens[3] == Token(WhitespaceNewline, Pos(2, 3)));
    ASSERT(tokens[4] == Token(SYM("ghi"), Pos(4, 0)));
    ASSERT(tokens[5] == Token(WhitespaceNewline, Pos(4, 3)));
    ASSERT(tokens.size() == 6);
}

TEST(lex_indentation_simple) {
    auto artifact = LEX(R"(
abc
    def
ghi
)");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("abc"), Pos(1, 0)));
    ASSERT(tokens[1] == Token(WhitespaceNewline, Pos(1, 3)));
    ASSERT(tokens[2] == Token(WhitespaceIndent, Pos(2, 4)));
    ASSERT(tokens[3] == Token(SYM("def"), Pos(2, 4)));
    ASSERT(tokens[4] == Token(WhitespaceNewline, Pos(2, 7)));
    ASSERT(tokens[5] == Token(WhitespaceDedent, Pos(3, 0)));
    ASSERT(tokens[6] == Token(SYM("ghi"), Pos(3, 0)));
    ASSERT(tokens[7] == Token(WhitespaceNewline, Pos(3, 3)));
    ASSERT(tokens.size() == 8);
}

TEST(lex_indentation_nested) {
    auto artifact = LEX(R"(
a
 b
  c
 d
  e
 f
g
)");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("a"), Pos(1, 0)));
    ASSERT(tokens[1] == Token(WhitespaceNewline, Pos(1, 1)));
    ASSERT(tokens[2] == Token(WhitespaceIndent, Pos(2, 1)));
    ASSERT(tokens[3] == Token(SYM("b"), Pos(2, 1)));
    ASSERT(tokens[4] == Token(WhitespaceNewline, Pos(2, 2)));
    ASSERT(tokens[5] == Token(WhitespaceIndent, Pos(3, 2)));
    ASSERT(tokens[6] == Token(SYM("c"), Pos(3, 2)));
    ASSERT(tokens[7] == Token(WhitespaceNewline, Pos(3, 3)));
    ASSERT(tokens[8] == Token(WhitespaceDedent, Pos(4, 1)));
    ASSERT(tokens[9] == Token(SYM("d"), Pos(4, 1)));
    ASSERT(tokens[10] == Token(WhitespaceNewline, Pos(4, 2)));
    ASSERT(tokens[11] == Token(WhitespaceIndent, Pos(5, 2)));
    ASSERT(tokens[12] == Token(SYM("e"), Pos(5, 2)));
    ASSERT(tokens[13] == Token(WhitespaceNewline, Pos(5, 3)));
    ASSERT(tokens[14] == Token(WhitespaceDedent, Pos(6, 1)));
    ASSERT(tokens[15] == Token(SYM("f"), Pos(6, 1)));
    ASSERT(tokens[16] == Token(WhitespaceNewline, Pos(6, 2)));
    ASSERT(tokens[17] == Token(WhitespaceDedent, Pos(7, 0)));
    ASSERT(tokens[18] == Token(SYM("g"), Pos(7, 0)));
    ASSERT(tokens[19] == Token(WhitespaceNewline, Pos(7, 1)));
    ASSERT(tokens.size() == 20);
}

TEST(lex_starts_indented) {
    auto artifact = LEX(R"(
    abc
def
)");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(WhitespaceIndent, Pos(1, 4)));
    ASSERT(tokens[1] == Token(SYM("abc"), Pos(1, 4)));
    ASSERT(tokens[2] == Token(WhitespaceNewline, Pos(1, 7)));
    ASSERT(tokens[3] == Token(WhitespaceDedent, Pos(2, 0)));
    ASSERT(tokens[4] == Token(SYM("def"), Pos(2, 0)));
    ASSERT(tokens[5] == Token(WhitespaceNewline, Pos(2, 3)));
    ASSERT(tokens.size() == 6);
}

TEST(lex_ends_indented) {
    auto artifact = LEX(R"(
abc
    def
)");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("abc"), Pos(1, 0)));
    ASSERT(tokens[1] == Token(WhitespaceNewline, Pos(1, 3)));
    ASSERT(tokens[2] == Token(WhitespaceIndent, Pos(2, 4)));
    ASSERT(tokens[3] == Token(SYM("def"), Pos(2, 4)));
    ASSERT(tokens[4] == Token(WhitespaceNewline, Pos(2, 7)));
    ASSERT(tokens[5] == Token(WhitespaceDedent, Pos(3, 0)));
    ASSERT(tokens.size() == 6);
}

TEST(lex_multiple_simultaneous_dedent) {
    auto artifact = LEX(R"(
abc
    def
        ghi
jkl
)");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("abc"), Pos(1, 0)));
    ASSERT(tokens[1] == Token(WhitespaceNewline, Pos(1, 3)));
    ASSERT(tokens[2] == Token(WhitespaceIndent, Pos(2, 4)));
    ASSERT(tokens[3] == Token(SYM("def"), Pos(2, 4)));
    ASSERT(tokens[4] == Token(WhitespaceNewline, Pos(2, 7)));
    ASSERT(tokens[5] == Token(WhitespaceIndent, Pos(3, 8)));
    ASSERT(tokens[6] == Token(SYM("ghi"), Pos(3, 8)));
    ASSERT(tokens[7] == Token(WhitespaceNewline, Pos(3, 11)));
    ASSERT(tokens[8] == Token(WhitespaceDedent, Pos(4, 0)));
    ASSERT(tokens[9] == Token(WhitespaceDedent, Pos(4, 0)));
    ASSERT(tokens[10] == Token(SYM("jkl"), Pos(4, 0)));
    ASSERT(tokens[11] == Token(WhitespaceNewline, Pos(4, 3)));
    ASSERT(tokens.size() == 12);
}

TEST(lex_blank_line) {
    auto artifact = LEX(R"(
abc
    def

    ghi

jkl
)");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("abc"), Pos(1, 0)));
    ASSERT(tokens[1] == Token(WhitespaceNewline, Pos(1, 3)));
    ASSERT(tokens[2] == Token(WhitespaceIndent, Pos(2, 4)));
    ASSERT(tokens[3] == Token(SYM("def"), Pos(2, 4)));
    ASSERT(tokens[4] == Token(WhitespaceNewline, Pos(2, 7)));
    ASSERT(tokens[5] == Token(SYM("ghi"), Pos(4, 4)));
    ASSERT(tokens[6] == Token(WhitespaceNewline, Pos(4, 7)));
    ASSERT(tokens[7] == Token(WhitespaceDedent, Pos(6, 0)));
    ASSERT(tokens[8] == Token(SYM("jkl"), Pos(6, 0)));
    ASSERT(tokens[9] == Token(WhitespaceNewline, Pos(6, 3)));
    ASSERT(tokens.size() == 10);
}

TEST(lex_blank_line_and_comment) {
    auto artifact = LEX(R"(
abc
    # a comment

    def

    # another comment
    ghi

    # a third comment comment

jkl
)");
    const auto& tokens = artifact.as<Tokens>()->tokens;
    ASSERT(tokens[0] == Token(SYM("abc"), Pos(1, 0)));
    ASSERT(tokens[1] == Token(WhitespaceNewline, Pos(1, 3)));
    ASSERT(tokens[2] == Token(WhitespaceIndent, Pos(4, 4)));
    ASSERT(tokens[3] == Token(SYM("def"), Pos(4, 4)));
    ASSERT(tokens[4] == Token(WhitespaceNewline, Pos(4, 7)));
    ASSERT(tokens[5] == Token(SYM("ghi"), Pos(7, 4)));
    ASSERT(tokens[6] == Token(WhitespaceNewline, Pos(7, 7)));
    ASSERT(tokens[7] == Token(WhitespaceDedent, Pos(11, 0)));
    ASSERT(tokens[8] == Token(SYM("jkl"), Pos(11, 0)));
    ASSERT(tokens[9] == Token(WhitespaceNewline, Pos(11, 3)));
    ASSERT(tokens.size() == 10);
}