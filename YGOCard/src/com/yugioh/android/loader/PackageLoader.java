package com.yugioh.android.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.yugioh.android.classes.PackageItem;

import java.util.ArrayList;
import java.util.List;

public class PackageLoader extends BaseLoader<PackageItem> {

    public PackageLoader(Context context) {
        super(context);
    }

    @Override
    public List<PackageItem> loadInBackground() {

        List<PackageItem> list = new ArrayList<PackageItem>();
        list.add(new PackageItem(true, "", "第一期"));
        list.add(new PackageItem(false, "1", "VOL1"));
        list.add(new PackageItem(false, "2", "VOL2"));
        list.add(new PackageItem(false, "3", "VOL3"));
        list.add(new PackageItem(false, "4", "VOL4"));
        list.add(new PackageItem(true, "", "第二期"));
        list.add(new PackageItem(false, "5", "测试卡包1"));
        list.add(new PackageItem(false, "6", "测试卡包2"));


        return list;
        // return YGOAPI.getPackageList();
    }
}
