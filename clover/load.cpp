#include "clover/load.h"
#include "util/config.h"

namespace clover {
    Source::Source(Artifact* artifact, const_slice<i8> source):
        ArtifactData(artifact), text(source) {}

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
        Path path(file);
        return clover::addSourceFile(compilation, path);
    }

    Artifact* addSourceFile(Compilation* compilation, const Path& path) {
        Path parent = path.parent();
        slice<Directory*> searchDirectories = compilation->searchDirectories;

        if UNLIKELY(config::verboseSearchDirectories)
            println("[FILE]\tLooking for file at path ", path.to_bytes());

        // First, we want to see if any directory already has the artifact. We
        // iterate over each search directory, and try and find an existing
        // directory for each parent of the provided path. At this stage, if a
        // directory does not yet exist, we don't create it.

        Symbol artifactName = compilation->sym(basename(path.segments.back()));

        for (Directory* directory : searchDirectories) {
            bool doesntHave = false;
            for (auto s : parent.segments) {
                Symbol sym = compilation->sym(s);
                directory = directory->directoryByName(sym);
                if (directory == nullptr) {
                    doesntHave = true;
                    break;
                }
            }
            if (doesntHave)
                continue;

            if (auto artifact = directory->artifactByName(artifactName))
                return artifact;
        }

        // Otherwise, we see if a satisfying file exists at that relative path
        // in any search directory. If so, that's the one we use.

        constexpr u32 BufferSize = 8192;
        array<i8, BufferSize> buffer;
        slice<i8> wippath;
        Directory* foundDir = nullptr;
        file::FileInfo info;
        for (Directory* directory : searchDirectories) {
            wippath = buffer;
            vec<Directory*> searchPath;
            while (directory->parent) {
                searchPath.push(directory);
                directory = directory->parent;
            }
            for (Directory* dir : reversed(searchPath))
                wippath = format(wippath, "/", compilation->str(dir->name));
            wippath = path.write_bytes(wippath);
            info = file::pathinfo(buffer[{0, wippath.size()}]);
            if UNLIKELY(config::verboseSearchDirectories)
                println("[FILE]\t - Searching for file at path ", buffer[{0, wippath.size()}]);
            if (info.kind == file::FILE) {
                for (auto s : parent.segments)
                    directory = directory->ensureDirectoryByName(compilation->sym(s));
                foundDir = directory;
                if UNLIKELY(config::verboseSearchDirectories)
                    println("[FILE]\t   - Found file at path ", buffer[{0, wippath.size()}]);
                break;
            }
            memory::fill(buffer.begin(), 0, BufferSize);
        }

        if (!foundDir)
            return nullptr;

        slice<i8> text = { new i8[info.size], info.size };

        auto f = file::openbuf(buffer[{0, wippath.size()}], file::READ);
        assert(f != -1);

        file::readbuf(f, text);
        file::closebuf(f);

        Artifact* newArtifact = new Artifact(foundDir, artifactName);
        Source* source = new Source(newArtifact, text);
        newArtifact->filename = path.segments.back();
        newArtifact->update(source);
        foundDir->addArtifact(artifactName, newArtifact);

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

        Artifact* newArtifact = new Artifact(directory, artifactName);
        Source* source = new Source(newArtifact, contents.dup());
        newArtifact->update(source);

        if UNLIKELY(config::printSource)
            source->print(compilation);

        directory->addArtifact(artifactName, newArtifact);

        return newArtifact;
    }
}