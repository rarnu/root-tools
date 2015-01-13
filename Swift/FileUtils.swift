import UIKit

class FileUtils: NSObject {
    
    class func getDocumentPath() -> String {
        var paths = NSSearchPathForDirectoriesInDomains(NSSearchPathDirectory.DocumentDirectory, NSSearchPathDomainMask.AllDomainsMask, true);
        return paths[0] as String
    }
    
    class func makeDir(dir: String, basePath path: String) -> String {
        var pathTmp = "\(path)/\(dir)"
        var fmgr = NSFileManager.defaultManager()
        
        if (!fmgr.fileExistsAtPath(pathTmp)) {
            fmgr.createDirectoryAtPath(pathTmp, withIntermediateDirectories:true, attributes:nil, error:nil)
        }
        return pathTmp
    }
    
    class func writeTextFile(fileName: String, savePath path: String, fileContent text: String) {
        var document = self.getDocumentPath()
        var pathTmp = self.makeDir(path, basePath:document)
        var fileOper = "\(pathTmp)/\(fileName)"
        var writeData = (text as NSString).dataUsingEncoding(NSUTF8StringEncoding)
        writeData!.writeToFile(fileOper, atomically:true)
    }
    
    class func readTextFile(fileName: String, loadPath path: String) -> String {
        var document = self.getDocumentPath()
        var pathTemp = "\(document)/\(path)"
        var fileOper = "\(pathTemp)/\(fileName)"
    
        var text: String = ""
        if (self.fileExists(fileName, filePath:path)) {
            var readData = NSData(contentsOfFile: fileOper)
            text = NSString(data: readData!, encoding: NSUTF8StringEncoding) as String
        }
        return text
    }
    
    class func writeFile(fileName: String, savePath path: String, fileData data: NSData) {
        var document = self.getDocumentPath()
        var pathTmp = self.makeDir(path, basePath:document)
        var fileOper = "\(pathTmp)/\(fileName)"
        data.writeToFile(fileOper, atomically:true)
    }
    
    class func readFile(fileName: String, loadPath path: String) -> NSData? {
        var document = self.getDocumentPath()
        var pathTemp = "\(document)/\(path)"
        var fileOper = "\(pathTemp)/\(fileName)"
        var retData: NSData? = nil
        if (self.fileExists(fileName, filePath:path)) {
            retData = NSData(contentsOfFile: fileOper)
        }
        return retData
    }
    
    class func fileExists(fileName: String, filePath path: String) -> Bool {
        var document = self.getDocumentPath()
        var pathTemp = "\(document)/\(path)"
        var fileOper = "\(pathTemp)/\(fileName)"
        var fmgr = NSFileManager.defaultManager()
        return fmgr.fileExistsAtPath(fileOper)
    }
    
    class func fileSizeAtPath(filePath: String) -> UInt64 {
        var manager = NSFileManager.defaultManager()
        if (manager.fileExistsAtPath(filePath)) {
            var attrs = manager.attributesOfItemAtPath(filePath, error: nil)
            var dic = NSDictionary(dictionary: attrs!)
            return dic.fileSize()
        }
        return 0
    }
    
    class func folderSizeAtPath(folderPath: String) -> Double {
        var manager = NSFileManager.defaultManager()
        if (!manager.fileExistsAtPath(folderPath)) {
            return 0
        }
        var childFilesEnumerator = manager.subpathsAtPath(folderPath)
        var folderSize: UInt64 = 0
        if (childFilesEnumerator != nil) {
            for fileName in childFilesEnumerator! {
                var fileAbsolutePath = "\(folderPath)/\(fileName)"
                folderSize += self.fileSizeAtPath(fileAbsolutePath)
            }
        }
        // Byte to M
        var fz: Double = Double(folderSize)
        return (fz / 1024.0 / 1024.0)
    }
   
}
