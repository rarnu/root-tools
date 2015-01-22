#import "FileUtils.h"

@implementation FileUtils

+(NSString *) getDocumentPath {
    NSArray * paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSAllDomainsMask, YES);
    return paths[0];
}

+(BOOL) makeDir: (NSString *)dir {
    BOOL ret = NO;
    NSFileManager * fmgr = [NSFileManager defaultManager];
    if (![fmgr fileExistsAtPath: dir]) {
        ret = [fmgr createDirectoryAtPath:dir withIntermediateDirectories:YES attributes:nil error:nil];
    }
    return ret;
}

+(NSString *) makeDir: (NSString *)dir basePath: (NSString *)path {
    BOOL ret = NO;
    NSString * pathTmp = [NSString stringWithFormat:@"%@/%@", path, dir];
    NSFileManager * fmgr = [NSFileManager defaultManager];
    if (![fmgr fileExistsAtPath: pathTmp]) {
        ret = [fmgr createDirectoryAtPath:pathTmp withIntermediateDirectories:YES attributes:nil error:nil];
    }
    return pathTmp;
}

+(NSString *) buildPath: (NSString *)fileName path: (NSString *)path {
    NSString * document = [self getDocumentPath];
    NSString * pathTmp = [self makeDir: path basePath:document];
    NSString * fileOper = [NSString stringWithFormat:@"%@/%@", pathTmp, fileName];
    return fileOper;
}

+(NSString *) getLocalFile: (NSString *)name type: (NSString *)type {
    return [[NSBundle mainBundle] pathForResource:name ofType:type];
}

+(NSString *) getLocalFile: (NSString *)name type: (NSString *)type dir: (NSString *)dir {
    return [[NSBundle mainBundle] pathForResource:name ofType:type inDirectory:dir];
}

+(void) rewriteTextFile:(NSString *)fileName savePath: (NSString *)path fileContent: (NSString *)text {
    NSString * fileOper = [self buildPath:fileName path: path];
    NSData * writeData = [text dataUsingEncoding: NSUTF8StringEncoding];
    [writeData writeToFile:fileOper atomically:YES];
}

+(void) appendTextFile: (NSString *)fileName savePath: (NSString *)path fileContent: (NSString *)text {
    NSString * fileOper = [self buildPath:fileName path:path];
    NSFileHandle * file = [NSFileHandle fileHandleForWritingAtPath:fileOper];
    if (file != nil) {
        [file seekToEndOfFile];
        NSData * writeData = [text dataUsingEncoding: NSUTF8StringEncoding];
        [file writeData: writeData];
        [file closeFile];
    } else {
        [self rewriteTextFile:fileName savePath:path fileContent:text];
    }
}

+(NSString *) readTextFile: (NSString *)fileName loadPath: (NSString *)path {
    NSString * fileOper = [self buildPath:fileName path:path];
    NSMutableString * text = [[NSMutableString alloc] initWithString:@""];
    if ([self fileExists:fileName filePath:path]) {
        NSData * readData = [NSData dataWithContentsOfFile:fileOper];
        text = [[NSMutableString alloc] initWithData:readData encoding:NSUTF8StringEncoding];
    }
    return text;
}

+(void) rewriteFile:(NSString *)fileName savePath: (NSString *)path fileData: (NSData *)data {
    NSString * fileOper = [self buildPath:fileName path:path];
    [data writeToFile:fileOper atomically:YES];
}

+(void) appendFile: (NSString *)fileName savePath: (NSString *)path fileData: (NSData *)data {
    NSString * fileOper = [self buildPath:fileName path:path];
    NSFileHandle * file = [NSFileHandle fileHandleForWritingAtPath:fileOper];
    if (file != nil) {
        [file seekToEndOfFile];
        [file writeData: data];
        [file closeFile];
    } else {
        [self rewriteFile:fileName savePath:path fileData:data];
    }
}

+(NSData *) readFile: (NSString *)fileName loadPath: (NSString *)path {
    NSString * fileOper = [self buildPath:fileName path:path];
    NSData * retData = nil;
    if ([self fileExists:fileName filePath:path]) {
        retData = [NSData dataWithContentsOfFile:fileOper];
    }
    return retData;
}

+(BOOL) deleteFile: (NSString *)fileName path: (NSString *)path {
    BOOL ret = NO;
    NSString * fileOper = [self buildPath:fileName path:path];
    NSFileManager * fmgr = [NSFileManager defaultManager];
    ret = [fmgr removeItemAtPath:fileOper error:nil];
    return ret;
}

+(BOOL) deleteDir: (NSString *)dir path: (NSString *)path {
    BOOL ret = NO;
    NSString * fileOper = [self buildPath:dir path:path];
    NSFileManager * fmgr = [NSFileManager defaultManager];
    ret = [fmgr removeItemAtPath:fileOper error:nil];
    return ret;
}

+(BOOL) copyFile: (NSString *)sourceFile sourcePath: (NSString *)sourcePath destFile: (NSString *)destFile destPath: (NSString *)destPath {
    BOOL ret = NO;
    NSString * fileSource = [self buildPath:sourceFile path:sourcePath];
    NSString * fileDest = [self buildPath:destFile path:destPath];
    NSFileManager * fmgr = [NSFileManager defaultManager];
    ret = [fmgr copyItemAtPath:fileSource toPath:fileDest error:nil];
    return ret;
}

+(BOOL) copyFolder: (NSString *)sourceFolder sourcePath: (NSString *)sourcePath destFolder: (NSString *)destFolder destPath: (NSString *)destPath {
    BOOL ret = NO;
    NSString * fileSource = [self buildPath:sourceFolder path:sourcePath];
    NSString * fileDest = [self buildPath:destFolder path:destPath];
    NSFileManager * fmgr = [NSFileManager defaultManager];
    ret = [fmgr copyItemAtPath:fileSource toPath:fileDest error:nil];
    return ret;
}

+(BOOL) moveFile: (NSString *)sourceFile sourcePath: (NSString *)sourcePath destFile: (NSString *)destFile destPath: (NSString *)destPath {
    BOOL ret = NO;
    NSFileManager * fmgr = [NSFileManager defaultManager];
    NSString * fileSource = [self buildPath:sourceFile path:sourcePath];
    NSString * fileDest = [self buildPath:destFile path:destPath];
    ret = [fmgr moveItemAtPath:fileSource toPath:fileDest error:nil];
    return ret;
}

+(BOOL) moveFolder: (NSString *)sourceFolder sourcePath: (NSString *)sourcePath destFolder: (NSString *)destFolder destPath: (NSString *)destPath {
    BOOL ret = NO;
    NSFileManager * fmgr = [NSFileManager defaultManager];
    NSString * fileSource = [self buildPath:sourceFolder path:sourcePath];
    NSString * fileDest = [self buildPath:destFolder path:destPath];
    ret  =[fmgr moveItemAtPath:fileSource toPath:fileDest error:nil];
    return ret;
}

+(NSArray *) loadArrayFromFile: (NSString *)fileName path: (NSString *)path {
    NSString * fileOper = [self buildPath:fileName path:path];
    NSArray * arr = [NSArray arrayWithContentsOfFile:fileOper];
    return arr;
}

+(BOOL) saveArrayToFile: (NSString *)fileName path: (NSString *)path array: (NSArray *)array {
    BOOL ret = NO;
    if (array != nil) {
        NSString * fileOper = [self buildPath:fileName path:path];
        ret = [array writeToFile:fileOper atomically:YES];
    }
    return ret;
}

+(BOOL) fileExists: (NSString *)fileName filePath: (NSString *)path {
    NSString * fileOper = [self buildPath:fileName path:path];
    NSFileManager * fmgr = [NSFileManager defaultManager];
    return [fmgr fileExistsAtPath: fileOper];
}

+(BOOL) fileExists: (NSString *)filePath {
    return [[NSFileManager defaultManager] fileExistsAtPath: filePath];
}

+(NSInteger) getFileSize: (NSString *)filePath {
    NSFileManager * fmgr = [NSFileManager defaultManager];
    if ([fmgr fileExistsAtPath: filePath]) {
        NSDictionary * attrs = [fmgr attributesOfItemAtPath:filePath error:nil];
        return (NSInteger)[attrs fileSize];
    }
    return 0;
}

+(NSInteger) getDirSize: (NSString *)folderPath {
    NSFileManager * fmgr = [NSFileManager defaultManager];
    if (![fmgr fileExistsAtPath: folderPath]) {
        return 0;
    }
    NSArray * childFilesEnumerator = [fmgr subpathsAtPath: folderPath];
    NSInteger folderSize = 0;
    if (childFilesEnumerator != nil) {
        for (NSString * fileName in childFilesEnumerator) {
            NSString * fileAbsolutePath = [NSString stringWithFormat:@"%@/%@", folderPath, fileName];
            folderSize += [self getFileSize: fileAbsolutePath];
        }
    }
    return folderSize;
}

+(NSString *) getReadableFileSize: (NSInteger)fileSize {
    double fz = fileSize / 1024.0;
    return [NSString stringWithFormat:@"%f KB", fz];
}


@end
