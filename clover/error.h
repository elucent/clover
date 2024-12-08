#ifndef CLOVER_ERROR_H
#define CLOVER_ERROR_H

#include "clover/lex.h"
#include "util/str.h"

namespace clover {
    struct Note {
        Pos pos;
        const_slice<i8> message;

        inline virtual ~Note() {
            delete[] message.data();
        }

        inline Note(Pos pos_in, const_slice<i8> message_in):
            pos(pos_in), message(message_in) {}

        template<typename... Args>
        inline Note(Pos pos_in, const Args&... args):
            Note(pos_in, tostring(args...)) {}
    };

    struct Error : public Note {
        Error* next;
        vec<Note*, 2> notes;

        inline Error(Pos pos_in, const_slice<i8> message_in):
            Note(pos_in, message_in), next(nullptr) {}

        template<typename... Args>
        inline Error(Pos pos_in, const Args&... args):
            Error(pos_in, tostring(args...)) {}

        PREVENT_COPYING(Error);
        PREVENT_MOVING(Error);

        inline ~Error() {
            if (next)
                delete next;
            for (Note* note : notes) delete note;
        }

        template<typename PosLike, typename... Args>
        inline Error& note(PosLike pos, const Args&... args);
    };

    inline Pos getPos(AST ast) {
        assert(!ast.isLeaf());
        return ast.pos();
    }

    inline Pos getPos(Pos pos) {
        return pos;
    }

    inline Pos getPos(Token token) {
        return token.pos;
    }

    template<typename PosLike, typename... Args>
    inline Error& error(ArtifactData* module, PosLike pos, const Args&... args) {
        Error* error = new Error(getPos(pos), args...);
        module->addError(error);
        if UNLIKELY(config::reportErrorsImmediately)
            reportErrorsAndExit(module);
        return *error;
    }

    template<typename PosLike, typename... Args>
    inline Error& Error::note(PosLike pos, const Args&... args) {
        notes.push(new Note(pos, args...));
        return *this;
    }
}

#endif