#import <Foundation/Foundation.h>
#define CU_EQUAL 0
#define CU_LEFT_BIGGER_THAN_RIGHT 1
#define CU_LEFT_SMALLER_THAN_RIGHT -1

@class CompareUtils;
@protocol CompareDelegate <NSObject>
@optional
-(NSComparisonResult)onCompare:(CompareUtils *)utils left:(id)left right:(id)right;
@end

@interface CompareUtils : NSObject {
    NSComparator comp;
}

@property id delegate;
-(id)initWithDelegate:(id)delegate;
-(NSComparator)getComparator;

@end
