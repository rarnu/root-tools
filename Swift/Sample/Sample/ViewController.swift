//
//  ViewController.swift
//  Sample
//
//  Created by rarnu on 1/21/15.
//  Copyright (c) 2015 rarnu. All rights reserved.
//

import UIKit
import Utils

class ViewController: UIViewController {

    var jsonString = "{\"name\":\"aaa\",\"age\":100}";
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        var dc = DemoClass()

        JsonUtils.fillJsonToObject(dc, jsonString: jsonString)
        NSLog("dc: name=\(dc.name!), age=\(dc.age!)")
        NSLog("\(JsonUtils.objectToJsonString(dc))")
        
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }


}

