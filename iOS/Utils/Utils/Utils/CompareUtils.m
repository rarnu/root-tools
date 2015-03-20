#import "CompareUtils.h"

@implementation CompareUtils

static CompareUtils * _instance;

-(id)init {
    if (self) {
        _instance = self;
    }
    return self;
}

-(id)initWithDelegate:(id)delegate {
    if (self) {
        _instance = self;
        self.delegate = delegate;
    }
    return self;
}

-(NSComparator)getComparator {
    comp = ^(id obj1, id obj2) {
        NSComparisonResult ret = (NSComparisonResult)NSOrderedSame;
        if ([_instance.delegate respondsToSelector:@selector(onCompare:left:right:)]) {
            ret = [_instance.delegate onCompare:_instance left:obj1 right:obj2];
        }
        return ret;
    };
    return comp;
}

@end
