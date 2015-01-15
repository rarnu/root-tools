import UIKit

@objc
protocol DownloadUtilsDelegate: NSObjectProtocol {
    optional func downloadUtils(downloadUtils: DownloadUtils, beginDownload: Int);
    optional func downloadUtils(downloadUtils: DownloadUtils, progress: Int);
    optional func downloadUtils(downloadUtils: DownloadUtils, endDownload: Int);
    optional func downloadUtils(downloadUtils: DownloadUtils, fileSaved: NSString);
    optional func downloadUtils(downloadUtils: DownloadUtils, error: NSString);
}

class DownloadUtils: NSObject, HttpUtilsDelegate {

    var delegate: DownloadUtilsDelegate?
    var savePath: String?
    
    func download(url: String, fileName: String, path: String) {
        savePath = FileUtils.buildPath(fileName, path: path)
        var http = HttpUtils()
        http.delegate = self
        http.get(url)
    }
    
    func httpUtils(httpUtils: HttpUtils, receivedData data: NSData?) {
        // TODO: This is not correct
        self.delegate?.downloadUtils?(self, endDownload: data!.length)
        var ret = data!.writeToFile(savePath!, atomically: true)
        if (ret) {
            self.delegate?.downloadUtils?(self, fileSaved: NSString(string: savePath!))
        }
    }
    
    func httpUtils(httpUtils: HttpUtils, receivedError err: NSString) {
        self.delegate?.downloadUtils?(self, error: err)
    }
    
    func httpUtils(httpUtils: HttpUtils, receivedFileSize fileSize: Int64) {
        self.delegate?.downloadUtils?(self, beginDownload: 0)
    }
    
    func httpUtils(httpUtils: HttpUtils, receivedProgress progress: Int) {
        self.delegate?.downloadUtils?(self, progress: progress)
    }
    
}
