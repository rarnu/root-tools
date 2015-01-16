import UIKit

class FileUtils: NSObject {
    
    class func getDocumentPath() -> String {
        var paths = NSSearchPathForDirectoriesInDomains(NSSearchPathDirectory.DocumentDirectory, NSSearchPathDomainMask.AllDomainsMask, true);
        return paths[0] as String
    }
    
    class func makeDir(dir: String) -> Bool {
        var ret = false
        var fmgr = NSFileManager.defaultManager()
        if (!fmgr.fileExistsAtPath(dir)) {
            ret = fmgr.createDirectoryAtPath(dir, withIntermediateDirectories: true, attributes: nil, error: nil)
        }
        return ret
    }
    
    class func makeDir(dir: String, basePath path: String) -> String {
        var ret = false
        var pathTmp = "\(path)/\(dir)"
        var fmgr = NSFileManager.defaultManager()
        
        if (!fmgr.fileExistsAtPath(pathTmp)) {
            ret = fmgr.createDirectoryAtPath(pathTmp, withIntermediateDirectories:true, attributes:nil, error:nil)
        }
        return pathTmp
    }
    
    class func buildPath(fileName: String, path: String) -> String {
        var document = self.getDocumentPath()
        var pathTmp = self.makeDir(path, basePath:document)
        var fileOper = "\(pathTmp)/\(fileName)"
        return fileOper
    }
    
    class func getLocalFile(name: String, type: String) -> String? {
        return NSBundle.mainBundle().pathForResource(name, ofType: type)
    }
    
    class func getLocalFile(name: String, type: String, dir: String) -> String? {
        return NSBundle.mainBundle().pathForResource(name, ofType: type, inDirectory: dir)
    }
    
    class func rewriteTextFile(fileName: String, savePath path: String, fileContent text: String) {
        var fileOper = buildPath(fileName, path: path)
        var writeData = (text as NSString).dataUsingEncoding(NSUTF8StringEncoding)
        writeData!.writeToFile(fileOper, atomically:true)
    }
    
    class func appendTextFile(fileName: String, savePath path: String, fileContent text: String) {
        var fileOper = buildPath(fileName, path: path)
        var file = NSFileHandle(forWritingAtPath: fileOper)
        if (file != nil) {
            file!.seekToEndOfFile()
            var writeData = (text as NSString).dataUsingEncoding(NSUTF8StringEncoding)
            file!.writeData(writeData!)
            file!.closeFile()
        } else {
            rewriteTextFile(fileName, savePath: path, fileContent: text)
        }
    }
    
    class func readTextFile(fileName: String, loadPath path: String) -> String {
        var fileOper = buildPath(fileName, path: path)
        var text: String = ""
        if (self.fileExists(fileName, filePath:path)) {
            var readData = NSData(contentsOfFile: fileOper)
            text = NSString(data: readData!, encoding: NSUTF8StringEncoding) as String
        }
        return text
    }
    
    class func rewriteFile(fileName: String, savePath path: String, fileData data: NSData) {
        var fileOper = buildPath(fileName, path: path)
        data.writeToFile(fileOper, atomically:true)
    }
    
    class func appendFile(fileName: String, savePath path: String, fileData data: NSData) {
        var fileOper = buildPath(fileName, path: path)
        var file = NSFileHandle(forWritingAtPath: fileOper)
        if (file != nil) {
            file!.seekToEndOfFile()
            file!.writeData(data)
            file!.closeFile()
        } else {
            rewriteFile(fileName, savePath: path, fileData: data)
        }
    }
    
    class func readFile(fileName: String, loadPath path: String) -> NSData? {
        var fileOper = buildPath(fileName, path: path)
        var retData: NSData? = nil
        if (self.fileExists(fileName, filePath:path)) {
            retData = NSData(contentsOfFile: fileOper)
        }
        return retData
    }
    
    class func deleteFile(fileName: String, path: String) -> Bool {
        var ret = false
        var fileOper = buildPath(fileName, path: path)
        var fmgr = NSFileManager.defaultManager()
        ret = fmgr.removeItemAtPath(fileOper, error: nil)
        return ret
    }
    
    class func deleteDir(dir: String, path: String) -> Bool {
        var ret = false
        var fileOper = buildPath(dir, path: path)
        var fmgr = NSFileManager.defaultManager()
        ret = fmgr.removeItemAtPath(fileOper, error: nil)
        return ret
    }
    
    class func copyFile(sourceFile: String, sourcePath: String, destFile: String, destPath: String) -> Bool {
        var ret = false
        var fileSource = buildPath(sourceFile, path: sourcePath)
        var fileDest = buildPath(destFile, path: destPath)
        var fmgr = NSFileManager.defaultManager()
        ret = fmgr.copyItemAtPath(fileSource, toPath: fileDest, error: nil)
        return ret
    }
    
    class func copyFolder(sourceFolder: String, sourcePath: String, destFolder: String, destPath: String) -> Bool {
        var ret = false
        var fileSource = buildPath(sourceFolder, path: sourcePath)
        var fileDest = buildPath(destFolder, path: destPath)
        var fmgr = NSFileManager.defaultManager()
        ret = fmgr.copyItemAtPath(fileSource, toPath: fileDest, error: nil)
        return ret
    }
    
    class func moveFile(sourceFile: String, sourcePath: String, destFile: String, destPath: String) -> Bool {
        var ret = false
        var fmgr = NSFileManager.defaultManager()
        var fileSource = buildPath(sourceFile, path: sourcePath)
        var fileDest = buildPath(destFile, path: destPath)
        ret = fmgr.moveItemAtPath(fileSource, toPath: fileDest, error: nil)
        return ret
    }
    
    class func moveFolder(sourceFolder: String, sourcePath: String, destFolder: String, destPath: String) -> Bool {
        var ret = false
        var fmgr = NSFileManager.defaultManager()
        var fileSource = buildPath(sourceFolder, path: sourcePath)
        var fileDest = buildPath(destFolder, path: destPath)
        ret = fmgr.moveItemAtPath(fileSource, toPath: fileDest, error: nil)
        return ret
    }
    
    class func loadArrayFromFile(fileName: String, path: String) -> NSArray? {
        var fileOper = buildPath(fileName, path: path)
        var arr = NSArray(contentsOfFile: fileOper)
        return arr
    }
    
    class func saveArrayToFile(fileName: String, path: String, array: NSArray?) -> Bool {
        var ret = false
        if (array != nil) {
            var fileOper = buildPath(fileName, path: path)
            ret = array!.writeToFile(fileOper, atomically: true)
        }
        return ret
    }
    
    class func fileExists(fileName: String, filePath path: String) -> Bool {
        var fileOper = buildPath(fileName, path: path)
        var fmgr = NSFileManager.defaultManager()
        return fmgr.fileExistsAtPath(fileOper)
    }
    
    class func fileExists(filePath: String) -> Bool {
        return NSFileManager.defaultManager().fileExistsAtPath(filePath)
    }
    
    class func getFileSize(filePath: String) -> UInt64 {
        var manager = NSFileManager.defaultManager()
        if (manager.fileExistsAtPath(filePath)) {
            var attrs = manager.attributesOfItemAtPath(filePath, error: nil)
            var dic = NSDictionary(dictionary: attrs!)
            return dic.fileSize()
        }
        return 0
    }
    
    class func getDirSize(folderPath: String) -> UInt64 {
        var manager = NSFileManager.defaultManager()
        if (!manager.fileExistsAtPath(folderPath)) {
            return 0
        }
        var childFilesEnumerator = manager.subpathsAtPath(folderPath)
        var folderSize: UInt64 = 0
        if (childFilesEnumerator != nil) {
            for fileName in childFilesEnumerator! {
                var fileAbsolutePath = "\(folderPath)/\(fileName)"
                folderSize += self.getFileSize(fileAbsolutePath)
            }
        }
        // Byte to M
        return folderSize
        
    }
    
    class func getReadableFileSize(fileSize: UInt64) -> String {
        var fz: Double = Double(fileSize)
        return "\(fz / 1024.0) KB"
    }
   
}
