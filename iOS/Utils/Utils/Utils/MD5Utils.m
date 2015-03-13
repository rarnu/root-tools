#import "MD5Utils.h"

#define FileHashDefaultChunkSizeForReadingData 1024 * 8

@implementation MD5Utils

+(NSString*)getFileMD5WithPath:(NSString*)path {
    return (__bridge_transfer NSString *)fileMD5HashCreateWithPath((__bridge CFStringRef)path, FileHashDefaultChunkSizeForReadingData);
}

CFStringRef fileMD5HashCreateWithPath(CFStringRef filePath,size_t chunkSizeForReadingData) {
    
    CFStringRef result = NULL;
    CFReadStreamRef readStream = NULL;
    CFURLRef fileURL = CFURLCreateWithFileSystemPath(kCFAllocatorDefault, (CFStringRef)filePath, kCFURLPOSIXPathStyle, (Boolean)false);
    
    @try {
        readStream = CFReadStreamCreateWithFile(kCFAllocatorDefault, (CFURLRef)fileURL);
        bool didSucceed = (bool)CFReadStreamOpen(readStream);
        CC_MD5_CTX hashObject;
        CC_MD5_Init(&hashObject);
        if (!chunkSizeForReadingData) {
            chunkSizeForReadingData = FileHashDefaultChunkSizeForReadingData;
        }
        bool hasMoreData = true;
        while (hasMoreData) {
            uint8_t buffer[chunkSizeForReadingData];
            CFIndex readBytesCount = CFReadStreamRead(readStream,(UInt8 *)buffer,(CFIndex)sizeof(buffer));
            if (readBytesCount == -1) {
                break;
            }
            if (readBytesCount == 0) {
                hasMoreData = false;
                continue;
            }
            CC_MD5_Update(&hashObject,(const void *)buffer,(CC_LONG)readBytesCount);
        }
        didSucceed = !hasMoreData;
        unsigned char digest[CC_MD5_DIGEST_LENGTH];
        CC_MD5_Final(digest, &hashObject);
        char hash[2 * sizeof(digest) + 1];
        for (size_t i = 0; i < sizeof(digest); ++i) {
            snprintf(hash + (2 * i), 3, "%02x", (int)(digest[i]));
        }
        result = CFStringCreateWithCString(kCFAllocatorDefault,(const char *)hash,kCFStringEncodingUTF8);

    } @catch (NSException *) {
        // do nothing
    } @finally {
        if (readStream) {
            CFReadStreamClose(readStream);
            CFRelease(readStream);
        }
        if (fileURL) {
            CFRelease(fileURL);
        }
    }
    return result;
    
}

@end
