#import <Foundation/Foundation.h>

@interface CYDatetime : NSObject

@property (nonatomic, assign) NSInteger year;
@property (nonatomic, assign) NSInteger month;
@property (nonatomic, assign) NSInteger day;
- (NSDate *)convertDate;

@end

