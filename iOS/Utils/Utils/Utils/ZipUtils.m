#import "ZipUtils.h"

@implementation ZipUtils

-(void) unzip {
    [NSThread detachNewThreadSelector:@selector(doUncompress) toTarget:self withObject:nil];
}

-(void) doUncompress {
    BOOL b = NO;
    if ([self.delegate respondsToSelector:@selector(zipWillUnzip)]) {
        b = [self.delegate zipWillUnzip];
    }
    if (b) {
        ZipArchive * za = [[ZipArchive alloc] init];
        NSNumber * ret = [NSNumber numberWithBool:NO];
        if ([za UnzipOpenFile: self.archiveFile]) {
            BOOL succ = [za UnzipFileTo:self.extractPath overWrite:YES];
            [za UnzipCloseFile];
            ret = [NSNumber numberWithBool:succ];
        }
        dispatch_async(dispatch_get_main_queue(), ^{
            [self callback: ret];
        });
    }
}

-(void) callback: (NSNumber *)ret {
    if ([self.delegate respondsToSelector:@selector(ziputils:unzipCompleted:)]) {
        [self.delegate ziputils:self unzipCompleted:[ret boolValue]];
    }
}


@end
