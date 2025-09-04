#ifndef CLOVER_LOAD_H
#define CLOVER_LOAD_H

#include "clover/compilation.h"

namespace clover {
    struct Source : public ArtifactData {
        const_slice<i8> text;

        Source(const_slice<i8> source);
        ~Source() override;

        maybe<const_slice<i8>> getSource() override;
        maybe<const_slice<u32>> getLineOffsets() override;
        const_slice<i8> takeSource();
        void print(Compilation* compilation) override;
        u64 reportSize() override;
    };

    void findSources(Compilation* compilation, const_slice<i8> root);
    Artifact* addSourceFile(Compilation* compilation, const_slice<i8> file);
    Artifact* addSourceFile(Compilation* compilation, const Path& path);
    Artifact* addSourceString(Compilation* compilation, const_slice<i8> virtualPath, const_slice<i8> contents);
}

#endif