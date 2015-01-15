//
//  UIUtils.swift
//  YibanJob
//
//  Created by rarnu on 1/10/15.
//  Copyright (c) 2015 yiban. All rights reserved.
//

import UIKit

class UIUtils: NSObject {
    
    class func getDefaultColor() -> UIColor {
        return UIColor(red: 0x00, green: 0xA5 / 0xFF, blue: 0xED / 0xFF, alpha: 1)
    }
    
    class func getStatusBarSize() -> CGSize {
        return UIApplication.sharedApplication().statusBarFrame.size
    }
    
    class func getScreenSize() -> CGSize {
        return UIScreen.mainScreen().bounds.size
    }
    
    class func setNavBar(nav: UINavigationBar, bgColor: UIColor, textColor: UIColor) {
        nav.barTintColor = bgColor
        nav.tintColor = textColor
        nav.titleTextAttributes = NSDictionary(objectsAndKeys: textColor, NSForegroundColorAttributeName)
    }
    
    class func setTabBar(tab: UITabBar, bgColor: UIColor, textColor: UIColor) {
        tab.tintColor = textColor
        tab.barTintColor = bgColor
    }
    
    class func setStatusBarStyle(light: Bool) {
        UIApplication.sharedApplication().setStatusBarStyle(light ? UIStatusBarStyle.LightContent : UIStatusBarStyle.Default, animated: false)
    }

    class func setTextTitleForSideMenu(controller: UIViewController, txt: UILabel) -> (CGFloat, CGFloat) {
        var y = UIUtils.getStatusBarSize().height
        var w = UIUtils.getScreenSize().width
        var h: CGFloat = getStatusBarHeight()
        var t: CGFloat = y + h
        var rh: CGFloat = UIUtils.getScreenSize().height - t
        var backColor = getDefaultColor()
        txt.frame = CGRectMake(0, y, w, h)
        txt.backgroundColor = backColor
        txt.textColor = UIColor.whiteColor()
        h = UIUtils.getStatusBarSize().height
        var txtImmersion = UILabel(frame: CGRectMake(0, 0, w, h))
        txtImmersion.backgroundColor = backColor
        controller.view.addSubview(txtImmersion)
        return (t, rh)
    }
    
    class func getStatusBarHeight() -> CGFloat {
        return UIApplication.sharedApplication().statusBarFrame.height
    }
    
    class func getAppFrameSize() -> CGSize {
        return UIScreen.mainScreen().applicationFrame.size
    }
}
