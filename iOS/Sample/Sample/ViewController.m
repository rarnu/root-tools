//
//  ViewController.m
//  Sample
//
//  Created by rarnu on 3/12/15.
//  Copyright (c) 2015 YiBan. All rights reserved.
//

#import "ViewController.h"
#import <Utils/Toast.h>

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

-(IBAction)btnClicked:(id)sender {
    [[[Toast makeText:@"toast"] setGravity:ToastGravityBottom] show];
}

@end
