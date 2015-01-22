#import "DownloadUtils.h"

@implementation DownloadUtils

-(void) download: (NSString *)url name: (NSString *)name path: (NSString *)path {
    self.fileName = name;
    self.filePath = path;
    self.progress = 0;
    NSURL * downloadUrl = [NSURL URLWithString:url];
    NSURLRequest * req = [NSURLRequest requestWithURL:downloadUrl cachePolicy:NSURLRequestUseProtocolCachePolicy timeoutInterval:60];
    NSURLConnection * conn = [NSURLConnection connectionWithRequest:req delegate:self];
    [conn start];
}

-(void) connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    if ([FileUtils fileExists:self.fileName filePath:self.filePath]) {
        [FileUtils appendFile:self.fileName savePath:self.filePath fileData:data];
    } else {
        [FileUtils rewriteFile:self.fileName savePath:self.filePath fileData:data];
    }
    self.progress += data.length;
    if ([self.delegate respondsToSelector:@selector(downloadUtils:progress:)]) {
        [self.delegate downloadUtils:self progress:self.progress];
    }
}

-(void) connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    if ([self.delegate respondsToSelector:@selector(downloadUtils:beginDownload:)]) {
        [self.delegate downloadUtils:self beginDownload:0];
    }
}

-(void) connectionDidFinishLoading:(NSURLConnection *)connection {
    if ([self.delegate respondsToSelector:@selector(downloadUtils:endDownload:filePath:)]) {
        [self.delegate downloadUtils:self endDownload:self.progress filePath:[FileUtils buildPath:self.fileName path:self.filePath]];
    }
}

-(void) connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    if ([self.delegate respondsToSelector:@selector(downloadUtils:error:)]) {
        [self.delegate downloadUtils:self error:error.localizedDescription];
    }
}

-(void) connection:(NSURLConnection *)connection willSendRequestForAuthenticationChallenge:(NSURLAuthenticationChallenge *)challenge {
    NSURLCredential * cred = [NSURLCredential credentialForTrust:challenge.protectionSpace.serverTrust];
    [challenge.sender useCredential:cred forAuthenticationChallenge:challenge];
    [challenge.sender continueWithoutCredentialForAuthenticationChallenge:challenge];
}

@end
