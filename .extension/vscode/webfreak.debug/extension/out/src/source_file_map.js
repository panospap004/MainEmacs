"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.SourceFileMap = void 0;
const path_kind_1 = require("./path_kind");
class SourceFileMap {
    constructor(map, remoteCwd) {
        this.sortedMappings = { remote: [], local: [] };
        const mappings = [];
        this.remoteCwd = remoteCwd;
        this.nativePath = this.getNativePath();
        for (let [remotePrefix, localPrefix] of Object.entries(map)) {
            // Normalize local path, adding trailing separator if missing.
            localPrefix = this.nativePath.normalizeDir(localPrefix);
            // Try to detect remote path.
            const debuggerPath = this.toPathKind(remotePrefix);
            // Normalize remote path, adding trailing separator if missing.
            remotePrefix = debuggerPath.normalizeDir(remotePrefix);
            mappings.push({ remote: remotePrefix, local: localPrefix });
        }
        // Sort with longest paths first in case some paths are subsets, so that
        // we match the most appropriate (e.g., with path prefixes of '/home'
        // and '/home/foo', and a complete path of '/home/foo/bar.c', we should
        // match the '/home/foo' path prefix instead of '/home'.
        this.sortedMappings.local = [...mappings].sort((a, b) => b.local.length - a.local.length);
        this.sortedMappings.remote = [...mappings].sort((a, b) => b.remote.length - a.remote.length);
    }
    // The native path selection is isolated here to allow for easy unit testing
    // allowing non-native path types to be tested by overriding this method in
    // a subclass in the test harness.
    getNativePath() {
        if (process.platform == "win32")
            return path_kind_1.PathWin32.getInstance();
        else
            return path_kind_1.PathPosix.getInstance();
    }
    toPathKind(unknownPath) {
        const pathPosix = path_kind_1.PathPosix.getInstance();
        const pathWin32 = path_kind_1.PathWin32.getInstance();
        if (pathPosix.isAbsolute(unknownPath) ||
            (this.remoteCwd && pathPosix.isAbsolute(this.remoteCwd))) {
            return pathPosix;
        }
        else {
            return pathWin32;
        }
    }
    pathMatch(key, caseSensitive, path) {
        for (const mapping of this.sortedMappings[key]) {
            let matched;
            if (caseSensitive)
                matched = path.startsWith(mapping[key]);
            else
                matched = path.toLowerCase().startsWith(mapping[key].toLowerCase());
            if (matched)
                return mapping;
        }
        return undefined;
    }
    toLocalPath(remotePath) {
        // Try to detect remote path.
        const debuggerPath = this.toPathKind(remotePath);
        const normalizedRemotePath = debuggerPath.normalize(remotePath);
        const mapping = this.pathMatch("remote", debuggerPath.caseSensitive, normalizedRemotePath);
        if (mapping) {
            const pathSuffix = normalizedRemotePath.substring(mapping.remote.length);
            return this.nativePath.join(mapping.local, pathSuffix);
        }
        // No mapping found, so return unmapped path.
        return remotePath;
    }
    toRemotePath(localPath) {
        const normalizedLocalPath = this.nativePath.normalize(localPath);
        const mapping = this.pathMatch("local", this.nativePath.caseSensitive, normalizedLocalPath);
        if (mapping) {
            const pathSuffix = normalizedLocalPath.substring(mapping.local.length);
            // Try to detect remote path.
            const debuggerPath = this.toPathKind(mapping.remote);
            return debuggerPath.join(mapping.remote, pathSuffix);
        }
        // No mapping found, so return unmapped path.
        return localPath;
    }
}
exports.SourceFileMap = SourceFileMap;
//# sourceMappingURL=source_file_map.js.map