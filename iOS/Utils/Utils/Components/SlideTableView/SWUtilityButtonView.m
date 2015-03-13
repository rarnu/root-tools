#import "SWUtilityButtonView.h"
#import "SWUtilityButtonTapGestureRecognizer.h"

@interface SWUtilityButtonView()

@property (nonatomic, strong) NSLayoutConstraint *widthConstraint;
@property (nonatomic, strong) NSMutableArray *buttonBackgroundColors;

@end

@implementation SWUtilityButtonView

#pragma mark - SWUtilityButonView initializers

- (id)initWithUtilityButtons:(NSArray *)utilityButtons parentCell:(SWTableViewCell *)parentCell utilityButtonSelector:(SEL)utilityButtonSelector {
    self = [self initWithFrame:CGRectZero utilityButtons:utilityButtons parentCell:parentCell utilityButtonSelector:utilityButtonSelector];
    return self;
}

- (id)initWithFrame:(CGRect)frame utilityButtons:(NSArray *)utilityButtons parentCell:(SWTableViewCell *)parentCell utilityButtonSelector:(SEL)utilityButtonSelector {
    self = [super initWithFrame:frame];

    if (self) {
        self.translatesAutoresizingMaskIntoConstraints = NO;
        
        self.widthConstraint = [NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1.0 constant:0.0];
        self.widthConstraint.priority = UILayoutPriorityDefaultHigh;
        [self addConstraint:self.widthConstraint];
        
        _parentCell = parentCell;
        self.utilityButtonSelector = utilityButtonSelector;
        self.utilityButtons = utilityButtons;
    }
    
    return self;
}

#pragma mark Populating utility buttons

- (void)setUtilityButtons:(NSArray *)utilityButtons {
    [self setUtilityButtons:utilityButtons WithButtonWidth:kUtilityButtonWidthDefault];
}

- (void)setUtilityButtons:(NSArray *)utilityButtons WithButtonWidth:(CGFloat)width {
    for (UIButton *button in _utilityButtons) {
        [button removeFromSuperview];
    }
    
    _utilityButtons = [utilityButtons copy];
    
    if (utilityButtons.count) {
        NSUInteger utilityButtonsCounter = 0;
        UIView *precedingView = nil;
        
        for (UIButton *button in _utilityButtons) {
            [self addSubview:button];
            button.translatesAutoresizingMaskIntoConstraints = NO;
            
            if (!precedingView) {
                [self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[button]" options:0L metrics:nil views:NSDictionaryOfVariableBindings(button)]];
            } else {
                [self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[precedingView][button(==precedingView)]" options:0L metrics:nil views:NSDictionaryOfVariableBindings(precedingView, button)]];
            }
            
            [self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[button]|" options:0L metrics:nil views:NSDictionaryOfVariableBindings(button)]];
            
            SWUtilityButtonTapGestureRecognizer *utilityButtonTapGestureRecognizer = [[SWUtilityButtonTapGestureRecognizer alloc] initWithTarget:_parentCell action:_utilityButtonSelector];
            utilityButtonTapGestureRecognizer.buttonIndex = utilityButtonsCounter;
            [button addGestureRecognizer:utilityButtonTapGestureRecognizer];
            
            utilityButtonsCounter++;
            precedingView = button;
        }
        
        [self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"[precedingView]|" options:0L metrics:nil views:NSDictionaryOfVariableBindings(precedingView)]];
    }
    
    self.widthConstraint.constant = (width * utilityButtons.count);
    
    [self setNeedsLayout];
    
    return;
}

#pragma mark -

- (void)pushBackgroundColors {
    self.buttonBackgroundColors = [[NSMutableArray alloc] init];
    
    for (UIButton *button in self.utilityButtons) {
        [self.buttonBackgroundColors addObject:button.backgroundColor];
    }
}

- (void)popBackgroundColors {
    NSEnumerator *e = self.utilityButtons.objectEnumerator;
    
    for (UIColor *color in self.buttonBackgroundColors) {
        UIButton *button = [e nextObject];
        button.backgroundColor = color;
    }
    
    self.buttonBackgroundColors = nil;
}

@end

