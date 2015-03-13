#import "HttpUtils.h"

#define BOUNDARY_STR @"--"
#define RANDOM_ID_STR  @"_yiban_req_"

@implementation HttpUtils

+(NSArray *) sendRequestSync: (NSURLRequest *)req {
    NSHTTPURLResponse * resp;
    NSError * error;
    NSData * retData =[NSURLConnection sendSynchronousRequest:req returningResponse:&resp error:&error];
    NSArray * result = nil;
    if (resp.statusCode == 200) {
        result = [NSArray arrayWithObjects:retData, resp, error, nil];
    }
    return result;
}

+(NSURLRequest *) buildGetRequest:(NSString *) url {
    NSURL * u = [NSURL URLWithString: url];
    NSURLRequest * req = [NSURLRequest requestWithURL:u cachePolicy:NSURLRequestUseProtocolCachePolicy timeoutInterval:60];
    return req;
}

+ (NSString *)topString:(NSString *)uploadID uploadFile:(NSString *)uploadFile {
    NSMutableString *strM = [NSMutableString string];
    [strM appendFormat:@"%@%@\n", BOUNDARY_STR, RANDOM_ID_STR];
    [strM appendFormat:@"Content-Disposition: form-data; name=\"%@\"; filename=\"%@\"\r\n", uploadID, uploadFile];
    [strM appendFormat:@"Content-Type: */*\r\n\r\n"];
    return strM;
}

+ (NSString *)bottomString {
    NSMutableString *strM = [NSMutableString string];
    [strM appendFormat:@"%@%@--\n", BOUNDARY_STR, RANDOM_ID_STR];
    return strM;
}

+(void)fillPostHead: (NSMutableURLRequest *)req {
    NSString *strContentType = [NSString stringWithFormat:@"multipart/form-data; boundary=%@", RANDOM_ID_STR];
    [req setValue:strContentType forHTTPHeaderField:@"Content-Type"];
    req.HTTPMethod = @"POST";
}

+(void)buildPostParam: (NSMutableData *)data dict: (NSDictionary *)dict {
    if (dict != nil) {
        for (NSString * key in dict.allKeys) {
            [data appendData:[[NSString stringWithFormat:@"%@%@\r\n", BOUNDARY_STR, RANDOM_ID_STR]dataUsingEncoding:NSUTF8StringEncoding]];
            [data appendData:[[NSString stringWithFormat:@"Content-Disposition:form-data; name=\"%@\"\r\n\r\n", key] dataUsingEncoding:NSUTF8StringEncoding]];
            [data appendData:[[NSString stringWithFormat:@"%@\r\n", [dict objectForKey:key]] dataUsingEncoding:NSUTF8StringEncoding]];
        }
    }
}

+(void)buildPostFileParam: (NSMutableData *)data dict: (NSDictionary *)dict {
    if (dict != nil) {
        NSString * filePath;
        NSString * topStr;
        for (NSString * key in dict.allKeys) {
            filePath = [dict objectForKey:key];
            topStr = [HttpUtils topString:key uploadFile:[filePath lastPathComponent]];
            [data appendData:[topStr dataUsingEncoding:NSUTF8StringEncoding]];
            [data appendData:[NSData dataWithContentsOfFile:filePath]];
            [data appendData:[[NSString stringWithFormat:@"%@\r\n", [dict objectForKey:key]] dataUsingEncoding:NSUTF8StringEncoding]];
        }
    }
}

+(void)buildBottomSplitter: (NSMutableData *)data {
    [data appendData:[[HttpUtils bottomString] dataUsingEncoding:NSUTF8StringEncoding]];
}

+(void)fillPostLength: (NSMutableURLRequest *)req data: (NSMutableData *)data {
    NSString *strLength = [NSString stringWithFormat:@"%ld", (long)data.length];
    [req setValue:strLength forHTTPHeaderField:@"Content-Length"];
}

+(NSURLRequest *) buildPostRequest:(NSString *)url dict: (NSDictionary *)dict {
    NSURL * u = [NSURL URLWithString:url];
    NSMutableURLRequest * req = [NSMutableURLRequest requestWithURL:u cachePolicy:0 timeoutInterval:2.0f];
    [HttpUtils fillPostHead:req];
    NSMutableData *dataM = [NSMutableData data];
    [HttpUtils buildPostParam:dataM dict:dict];
    [HttpUtils buildBottomSplitter:dataM];
    req.HTTPBody = dataM;
    [HttpUtils fillPostLength:req data:dataM];
    return req;
}

-(void) get: (NSString *) url {
    NSURLRequest * req = [HttpUtils buildGetRequest:url];
    NSURLConnection * conn = [NSURLConnection connectionWithRequest:req delegate:self];
    [conn start];
}

+(NSArray *) getSync: (NSString *)url {
    NSURLRequest * req = [HttpUtils buildGetRequest:url];
    return [HttpUtils sendRequestSync:req];
}

-(void) post: (NSString *)url dict: (NSDictionary *)dict {
    NSURLRequest * req = [HttpUtils buildPostRequest:url dict:dict];
    NSURLConnection * conn = [NSURLConnection connectionWithRequest:req delegate:self];
    [conn start];
}

+(NSArray *) postSync: (NSString *)url dict: (NSDictionary *)dict {
    NSURLRequest * req = [HttpUtils buildPostRequest:url dict:dict];
    return [HttpUtils sendRequestSync:req];
}

+(NSURLRequest *) buildPostFileRequest:(NSString *)url param: (NSDictionary *)param files: (NSDictionary *)files {
    NSURL * u = [NSURL URLWithString:url];
    NSMutableURLRequest * req = [NSMutableURLRequest requestWithURL:u cachePolicy:0 timeoutInterval:2.0f];
    [HttpUtils fillPostHead:req];
    NSMutableData *dataM = [NSMutableData data];
    [HttpUtils buildPostParam:dataM dict:param];
    [HttpUtils buildPostFileParam:dataM dict:files];
    [HttpUtils buildBottomSplitter:dataM];
    req.HTTPBody = dataM;
    [HttpUtils fillPostLength:req data:dataM];
    return req;
}

-(void) postFile: (NSString *)url param: (NSDictionary *)param files: (NSDictionary *)files {
    NSURLRequest * req = [HttpUtils buildPostFileRequest:url param:param files:files];
    NSURLConnection * conn = [NSURLConnection connectionWithRequest:req delegate:self];
    [conn start];
}

+(NSArray *) postFileSync: (NSString *)url param: (NSDictionary *)param files: (NSDictionary *)files {
    NSURLRequest * req = [HttpUtils buildPostFileRequest:url param:param files:files];
    return [HttpUtils sendRequestSync:req];
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
