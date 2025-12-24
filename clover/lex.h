#ifndef CLOVER_LEX_H
#define CLOVER_LEX_H

#include "clover/compilation.h"
#include "clover/load.h"
#include "util/sym.h"

namespace clover {
    struct Pos {
        static constexpr const u32 MaximumLine = (1 << 20) - 1;
        static constexpr const u32 MaximumColumn = (1 << 12) - 1;

        union {
            struct { u32 line : 20, column : 12; };
            u32 bits;
        };

        inline Pos() = default;

        inline Pos(u32 line_in, u32 column_in):
            line(line_in), column(column_in) {
            assert(line_in <= MaximumLine);
            assert(column_in <= MaximumColumn);
        }

        inline bool operator==(const Pos& other) const {
            return line == other.line && column == other.column;
        }

        inline bool operator!=(const Pos& other) const {
            return line != other.line || column != other.column;
        }

        inline Pos withOffset(u32 columnShift) {
            return { line, column + columnShift };
        }

        inline u32 encode() {
            return bits;
        }

        static inline Pos decode(u32 u) {
            Pos pos;
            pos.bits = u;
            return pos;
        }
    };

    struct PosRange {
        Pos start, end;

        inline PosRange() {}

        inline PosRange(Pos start_in, Pos end_in):
            start(start_in), end(end_in) {}

        inline PosRange(PosRange start, Pos end): PosRange(start.start, end) {}
        inline PosRange(Pos start, PosRange end): PosRange(start, end.end) {}
        inline PosRange(PosRange start, PosRange end): PosRange(start.start, end.end) {}
    };

    struct Token {
        Symbol token;
        Pos pos;

        inline Token(Symbol token_in, Pos pos_in):
            token(token_in), pos(pos_in) {}

        inline bool operator==(const Token& other) const {
            return token == other.token && pos == other.pos;
        }

        inline bool operator!=(const Token& other) const {
            return token != other.token || pos != other.pos;
        }

        inline PosRange span(SymbolTable* syms) {
            return { pos, pos.withOffset(syms->strings[token.symbol].size()) };
        }
    };

    static_assert(sizeof(Token) == 8);

    struct Tokens : public ArtifactData {
        const_slice<i8> source;
        vec<Token, 32> tokens;
        vec<u32> lineOffsets;

        Tokens(Artifact* artifact, const_slice<i8> source);
        ~Tokens() override;

        maybe<const_slice<i8>> getSource() override;
        maybe<const_slice<u32>> getLineOffsets() override;
        const_slice<i8> takeSource();
        void print(Compilation* compilation) override;
        u64 reportSize() override;
    };

    NOINLINE Artifact* lex(Artifact* artifact);
}

#endif