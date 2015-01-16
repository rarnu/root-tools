import UIKit

@objc
protocol DownloadUtilsDelegate: NSObjectProtocol {
    optional func downloadUtils(downloadUtils: DownloadUtils, beginDownload: Int);
    optional func downloadUtils(downloadUtils: DownloadUtils, progress: Int);
    optional func downloadUtils(downloadUtils: DownloadUtils, endDownload: Int, filePath: String);
    optional func downloadUtils(downloadUtils: DownloadUtils, error: NSString);
}

class DownloadUtils: NSObject, NSURLConnectionDataDelegate, NSURLConnectionDelegate {

    var delegate: DownloadUtilsDelegate?
    var fileName: String?
    var filePath: String?
    var progress: Int?
    
    func download(url: String, name: String, path: String) {
        fileName = name
        filePath = path
        progress = 0
        var downloadUrl = NSURL(string: url)
        var req = NSURLRequest(URL: downloadUrl!, cachePolicy: NSURLRequestCachePolicy.UseProtocolCachePolicy, timeoutInterval: 60)
        var conn = NSURLConnection(request: req, delegate: self)
        conn!.start()
    }
    
    func connection(connection: NSURLConnection, didReceiveData data: NSData) {
        if (FileUtils.fileExists(fileName!, filePath: filePath!)) {
            FileUtils.appendFile(fileName!, savePath: filePath!, fileData: data)
        } else {
            FileUtils.rewriteFile(fileName!, savePath: filePath!, fileData: data)
        }
        progress! += data.length
        self.delegate?.downloadUtils?(self, progress: progress!)
    }
    
    func connection(connection: NSURLConnection, didReceiveResponse response: NSURLResponse) {
        self.delegate?.downloadUtils?(self, beginDownload: 0)
    }
    
    func connectionDidFinishLoading(connection: NSURLConnection) {
        self.delegate?.downloadUtils?(self, endDownload: progress!, filePath: FileUtils.buildPath(fileName!, path: filePath!))
    }
    
    func connection(connection: NSURLConnection, didFailWithError error: NSError) {
        self.delegate?.downloadUtils?(self, error: error.localizedDescription)
    }
    
    func connection(connection: NSURLConnection, canAuthenticateAgainstProtectionSpace protectionSpace: NSURLProtectionSpace) -> Bool {
        return protectionSpace.authenticationMethod == NSURLAuthenticationMethodServerTrust
    }
    
    func connection(connection: NSURLConnection, didReceiveAuthenticationChallenge challenge: NSURLAuthenticationChallenge) {
        if (challenge.protectionSpace.authenticationMethod == NSURLAuthenticationMethodServerTrust) {
            challenge.sender.useCredential(NSURLCredential(forTrust: challenge.protectionSpace.serverTrust), forAuthenticationChallenge: challenge)
        }
        challenge.sender.continueWithoutCredentialForAuthenticationChallenge(challenge)
    }
    
}
