#ifndef CLOVER_ERROR_H
#define CLOVER_ERROR_H

#include "clover/lex.h"
#include "clover/limits.h"
#include "util/str.h"
#include "util/config.h"

namespace clover {
    struct AST;
    struct ChangePosition;

    struct Note {
        ArtifactData* module;
        union {
            Pos pos;
            struct { NodeIndex node; i32 child; };
        };
        const_slice<i8> message;
        bool isNode;

        inline virtual ~Note() {
            delete[] message.data();
        }

        inline Note(ArtifactData* module_in, Pos pos_in, const_slice<i8> message_in):
            module(module_in), pos(pos_in), message(message_in), isNode(false) {}

        template<typename... Args>
        inline Note(ArtifactData* module_in, Pos pos_in, const Args&... args):
            Note(module_in, pos_in, tostring(args...)) {}

        inline Note(ArtifactData* module_in, AST node_in, const_slice<i8> message_in);
        template<typename... Args>
        inline Note(ArtifactData* module_in, AST node_in, const Args&... args);

        inline Note(ArtifactData* module_in, ChangePosition node_in, const_slice<i8> message_in);
        template<typename... Args>
        inline Note(ArtifactData* module_in, ChangePosition node_in, const Args&... args);
    };

    struct Error : public Note {
        Error* next;
        vec<Note*, 2> notes;

        inline Error(ArtifactData* module_in, Pos pos_in, const_slice<i8> message_in):
            Note(module_in, pos_in, message_in), next(nullptr) {}

        template<typename... Args>
        inline Error(ArtifactData* module_in, Pos pos_in, const Args&... args):
            Error(module_in, pos_in, tostring(args...)) {}

        inline Error(ArtifactData* module_in, AST node_in, const_slice<i8> message_in);
        template<typename... Args>
        inline Error(ArtifactData* module_in, AST node_in, const Args&... args);

        inline Error(ArtifactData* module_in, ChangePosition node_in, const_slice<i8> message_in);
        template<typename... Args>
        inline Error(ArtifactData* module_in, ChangePosition node_in, const Args&... args);

        PREVENT_COPYING(Error);
        PREVENT_MOVING(Error);

        inline ~Error() {
            if (next)
                delete next;
            for (Note* note : notes) delete note;
        }

        template<typename PosLike, typename... Args>
        inline Error& note(ArtifactData* module, PosLike pos, const Args&... args);
        template<typename... Args>
        inline Error& note(ArtifactData* module, AST ast, const Args&... args);
        template<typename... Args>
        inline Error& note(ArtifactData* module, ChangePosition ast, const Args&... args);
    };

    inline Pos getPos(Pos pos) {
        return pos;
    }

    inline Pos getPos(Token token) {
        return token.pos;
    }

    template<typename PosLike, typename... Args>
    inline Error& error(ArtifactData* module, PosLike pos, const Args&... args) {
        Error* error = new Error(module, getPos(pos), args...);
        module->addError(error);
        if UNLIKELY(config::reportErrorsImmediately)
            reportErrorsAndExit(module);
        return *error;
    }

    template<typename PosLike, typename... Args>
    inline Error& Error::note(ArtifactData* module, PosLike pos, const Args&... args) {
        notes.push(new Note(module, pos, args...));
        return *this;
    }

    template<typename... Args>
    inline Error& error(ArtifactData* module, AST ast, const Args&... args);
    template<typename... Args>
    inline Error& note(ArtifactData* module, AST ast, const Args&... args);
    template<typename... Args>
    inline Error& error(ArtifactData* module, ChangePosition ast, const Args&... args);
    template<typename... Args>
    inline Error& note(ArtifactData* module, ChangePosition ast, const Args&... args);
}

#endif