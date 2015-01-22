#import "HttpUtils.h"

@implementation HttpUtils

-(void) get: (NSString *) url {
    NSURL * u = [NSURL URLWithString: url];
    NSURLRequest * req = [NSURLRequest requestWithURL:u cachePolicy:NSURLRequestUseProtocolCachePolicy timeoutInterval:60];
    NSURLConnection * conn = [NSURLConnection connectionWithRequest:req delegate:self];
    [conn start];
}

-(void) post: (NSString *)url param: (NSString *)param {
    NSURL * u = [NSURL URLWithString:url];
    NSMutableURLRequest * req = [NSMutableURLRequest requestWithURL:u cachePolicy:NSURLRequestUseProtocolCachePolicy timeoutInterval:60];
    req.HTTPMethod = @"POST";
    NSData * data = [param dataUsingEncoding:NSUTF8StringEncoding];
    req.HTTPBody = data;
    NSURLConnection * conn = [NSURLConnection connectionWithRequest:req delegate:self];
    [conn start];
}

-(void) postFile: (NSString *)url param: (NSDictionary *)param fieldName: (NSString *) fieldName fileName: (NSString *)fileName mimeType: (NSString *)mimeType file: (NSData *)file {
    
    NSString * prefix = @"--";
    NSString * suffix = @"22b996c312d6";
    NSString * boundary = [NSString stringWithFormat:@"%@%@", prefix, suffix];
    
    NSMutableString * topStr = [[NSMutableString alloc] initWithString:@""];
    [topStr appendFormat:@"%@\r\n", boundary];
    [topStr appendFormat:@"Content-Disposition: form-data; name=\"%@\"; filename=\"%@\"; ", fieldName, fileName];
    if (param != nil) {
        NSArray * keys = param.allKeys;
        for (NSString * s in keys) {
            [topStr appendFormat:@"%@=\"%@\"; ", s, (NSString *)[param objectForKey:s]];
        }
    }
    [topStr appendString:@"\r\n"];
    [topStr appendFormat:@"Content-Type: %@\r\n\r\n", mimeType];
    
    NSString * bottomStr = [NSString stringWithFormat:@"\r\n%@--", boundary];
    
    NSMutableData * dataP = [[NSMutableData alloc] init];
    [dataP appendData:[topStr dataUsingEncoding:NSUTF8StringEncoding]];
    [dataP appendData:file];
    [dataP appendData:[bottomStr dataUsingEncoding:NSUTF8StringEncoding]];
    
    NSURL * u = [NSURL URLWithString:url];
    NSMutableURLRequest * req = [NSMutableURLRequest requestWithURL:u cachePolicy:NSURLRequestUseProtocolCachePolicy timeoutInterval:60];
    req.HTTPBody = dataP;
    req.HTTPMethod = @"POST";
    
    NSString * stringLength = [NSString stringWithFormat:@"%ld", (unsigned long)dataP.length];
    [req setValue:stringLength forHTTPHeaderField:@"Content-Length"];
    NSString * stringContentType = [NSString stringWithFormat:@"multipart/form-data; boundary=%@", suffix];
    [req setValue:stringContentType forHTTPHeaderField:@"Content-Type"];
    
    NSURLConnection * conn = [NSURLConnection connectionWithRequest:req delegate:self];
    [conn start];
}


-(void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    [self.receivedData appendData:data];
    if ([self.delegate respondsToSelector:@selector(httpUtils:receivedProgress:)]) {
        [self.delegate httpUtils:self receivedProgress:self.receivedData.length];
    }
}

-(void) connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    self.receivedData = [[NSMutableData alloc] init];
    if ([self.delegate respondsToSelector:@selector(httpUtils:receivedFileSize:)]) {
        [self.delegate httpUtils:self receivedFileSize:(NSInteger)response.expectedContentLength];
    }
}

-(void) connectionDidFinishLoading:(NSURLConnection *)connection {
    if ([self.delegate respondsToSelector:@selector(httpUtils:receivedData:)]) {
        [self.delegate httpUtils:self receivedData:self.receivedData];
    }
}

-(void) connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    NSLog(@"%@", error.localizedDescription);
    if ([self.delegate respondsToSelector:@selector(httpUtils:receivedError:)]) {
        [self.delegate httpUtils:self receivedError:error.localizedDescription];
    }
}

-(void) connection:(NSURLConnection *)connection willSendRequestForAuthenticationChallenge:(NSURLAuthenticationChallenge *)challenge {
    NSURLCredential * cred = [NSURLCredential credentialForTrust:challenge.protectionSpace.serverTrust];
    [challenge.sender useCredential:cred forAuthenticationChallenge:challenge];
    [challenge.sender continueWithoutCredentialForAuthenticationChallenge:challenge];
}

@end
