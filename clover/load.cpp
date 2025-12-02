#include "clover/load.h"
#include "util/config.h"

namespace clover {
    Source::Source(const_slice<i8> source):
        text(source) {}

    Source::~Source() {
        if (text.data())
            delete[] text.data();
    }

    void Source::print(Compilation* compilation) {
        println(text);
    }

    maybe<const_slice<i8>> Source::getSource() {
        return some<const_slice<i8>>(text);
    }

    maybe<const_slice<u32>> Source::getLineOffsets() {
        return none<const_slice<u32>>();
    }

    u64 Source::reportSize() {
        return sizeof(Source) + text.size();
    }

    const_slice<i8> Source::takeSource() {
        auto taken = text;
        text = { nullptr, 0 };
        return taken;
    }

    void findSources(Compilation* compilation, const_slice<i8> root) {

    }

    Artifact* addSourceFile(Compilation* compilation, const_slice<i8> file) {
        Path path(compilation->cwd);
        path.append(file);
        return clover::addSourceFile(compilation, path);
    }

    Artifact* addSourceFile(Compilation* compilation, const Path& path) {
        Path parent = path.parent();
        auto* directory = compilation->root;
        for (auto s : parent.segments) {
            assert(compilation == directory->compilation);
            Symbol sym = compilation->sym(s);
            directory = directory->ensureDirectoryByName(sym);
        }

        Symbol artifactName = compilation->sym(basename(path.segments.back()));
        if (auto artifact = directory->artifactByName(artifactName))
            return artifact;

        const_slice<i8> os_path = path.to_bytes();
        auto info = file::info(os_path);
        assert(info.kind == file::FILE);
        slice<i8> text = { new i8[info.size], info.size };

        auto f = file::openbuf(os_path, file::READ);
        assert(f != -1);

        file::readbuf(f, text);
        file::closebuf(f);

        Source* source = new Source(text);
        Artifact* newArtifact = new Artifact(directory, artifactName);
        newArtifact->filename = path.segments.back();
        newArtifact->update(source);
        directory->addArtifact(artifactName, newArtifact);
        delete[] os_path.data();

        return newArtifact;
    }

    Artifact* addSourceString(Compilation* compilation, const_slice<i8> virtualPath, const_slice<i8> contents) {
        Path path(virtualPath);
        Path parent = path.parent();
        auto* directory = compilation->root;
        for (auto s : parent.segments) {
            assert(compilation == directory->compilation);
            Symbol sym = compilation->sym(s);
            directory = directory->ensureDirectoryByName(sym);
        }

        Symbol artifactName = compilation->sym(basename(path.segments.back()));
        assert(directory->artifactByName(artifactName) == nullptr);

        Source* source = new Source(contents.dup());
        Artifact* newArtifact = new Artifact(directory, artifactName);
        newArtifact->update(source);

        if UNLIKELY(config::printSource)
            source->print(compilation);

        directory->addArtifact(artifactName, newArtifact);

        return newArtifact;
    }
}