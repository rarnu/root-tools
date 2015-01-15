import UIKit

@objc
protocol ZipUtilsDelegate: NSObjectProtocol {
    optional func zipWillUnzip() -> Bool
    optional func ziputils(ziputils: ZipUtils, unzipCompleted succ: Bool)

}

class ZipUtils: NSObject {
    var delegate: ZipUtilsDelegate?
    var archiveFile: String?
    var extractPath: String?
    
    func unzip() {
        NSThread.detachNewThreadSelector("doUncompress", toTarget: self, withObject: nil)
    }
    
    func doUncompress() {
        var b = false
        if (self.delegate?.zipWillUnzip != nil) {
            b = self.delegate!.zipWillUnzip!()
        }
        if (b) {
            var za = ZipArchive()
            var ret = NSNumber(bool: false)
            if (za.UnzipOpenFile(self.archiveFile!)) {
                var succ = za.UnzipFileTo(self.extractPath, overWrite:true)
                za.UnzipCloseFile()
                ret = NSNumber(bool: succ)
            }
            dispatch_async(dispatch_get_main_queue(), {
                self.callback(ret)
            })
        }
    }
    
    func callback(ret: NSNumber) {
        self.delegate?.ziputils?(self, unzipCompleted: ret.boolValue)
    }

}
