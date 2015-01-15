import UIKit

class JailbreakUtils: NSObject {
    class func isJailbreaked() -> Bool {
        var ret = false
        var jailBreakFiles = [
            "/Application/Cydia.app",
            "/Application/limera1n.app",
            "/Application/greenpois0n.app",
            "/Application/blackra1n.app",
            "/Application/blacksn0w.app",
            "/application/redsn0w.app"
        ]
        var fmgr = NSFileManager.defaultManager()
        for file in jailBreakFiles {
            if (fmgr.fileExistsAtPath(file)) {
                ret = true
                break
            }
        }
        return ret
    }
}
