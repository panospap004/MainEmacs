"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.PathPosix = exports.PathWin32 = exports.PathKind = void 0;
const Path = require("path");
class PathKind {
    // The path.posix.normalize routine will not convert Win32 path separators
    // to POSIX separators, so we explictily convert any Win32 path separators
    // to POSIX style separators.  The path.win32.normalize routine will accept
    // either Win32 or POSIX style separators and will normalize them to the
    // Win32 style.  Thus, if we convert all path separators to POSIX style and
    // then normalize, this will work for both systems.
    normalize(p) {
        return this.path.normalize(p.replace(/\\/g, "/"));
    }
    normalizeDir(p) {
        p = this.normalize(p);
        if (!p.endsWith(this.path.sep))
            p = this.path.join(p, this.path.sep);
        return p;
    }
    join(...paths) {
        return this.normalize(this.path.join(...paths));
    }
    isAbsolute(p) {
        return this.path.isAbsolute(this.normalize(p));
    }
}
exports.PathKind = PathKind;
class PathWin32 extends PathKind {
    constructor() {
        super();
        this.path = Path.win32;
        this.caseSensitive = false;
    }
    static getInstance() {
        if (!this.instance)
            this.instance = new PathWin32();
        return this.instance;
    }
}
exports.PathWin32 = PathWin32;
class PathPosix extends PathKind {
    constructor() {
        super();
        this.path = Path.posix;
        this.caseSensitive = true;
    }
    static getInstance() {
        if (!this.instance)
            this.instance = new PathPosix();
        return this.instance;
    }
}
exports.PathPosix = PathPosix;
//# sourceMappingURL=path_kind.js.map