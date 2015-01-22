#import <Foundation/Foundation.h>

@interface FileUtils : NSObject

+(NSString *) getDocumentPath;
+(BOOL) makeDir: (NSString *)dir;
+(NSString *) makeDir: (NSString *)dir basePath: (NSString *)path;
+(NSString *) buildPath: (NSString *)fileName path: (NSString *)path;
+(NSString *) getLocalFile: (NSString *)name type: (NSString *)type;
+(NSString *) getLocalFile: (NSString *)name type: (NSString *)type dir: (NSString *)dir;
+(void) rewriteTextFile:(NSString *)fileName savePath: (NSString *)path fileContent: (NSString *)text;
+(void) appendTextFile: (NSString *)fileName savePath: (NSString *)path fileContent: (NSString *)text;
+(NSString *) readTextFile: (NSString *)fileName loadPath: (NSString *)path;
+(void) rewriteFile:(NSString *)fileName savePath: (NSString *)path fileData: (NSData *)data;
+(void) appendFile: (NSString *)fileName savePath: (NSString *)path fileData: (NSData *)data;
+(NSData *) readFile: (NSString *)fileName loadPath: (NSString *)path;
+(BOOL) deleteFile: (NSString *)fileName path: (NSString *)path;
+(BOOL) deleteDir: (NSString *)dir path: (NSString *)path;
+(BOOL) copyFile: (NSString *)sourceFile sourcePath: (NSString *)sourcePath destFile: (NSString *)destFile destPath: (NSString *)destPath;
+(BOOL) copyFolder: (NSString *)sourceFolder sourcePath: (NSString *)sourcePath destFolder: (NSString *)destFolder destPath: (NSString *)destPath;
+(BOOL) moveFile: (NSString *)sourceFile sourcePath: (NSString *)sourcePath destFile: (NSString *)destFile destPath: (NSString *)destPath;
+(BOOL) moveFolder: (NSString *)sourceFolder sourcePath: (NSString *)sourcePath destFolder: (NSString *)destFolder destPath: (NSString *)destPath;
+(NSArray *) loadArrayFromFile: (NSString *)fileName path: (NSString *)path;
+(BOOL) saveArrayToFile: (NSString *)fileName path: (NSString *)path array: (NSArray *)array;
+(BOOL) fileExists: (NSString *)fileName filePath: (NSString *)path;
+(BOOL) fileExists: (NSString *)filePath;
+(NSInteger) getFileSize: (NSString *)filePath;
+(NSInteger) getDirSize: (NSString *)folderPath;
+(NSString *) getReadableFileSize: (NSInteger)fileSize;

@end
