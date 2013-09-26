package com.rarnu.tools.root.utils;

import android.os.Handler;
import android.widget.BaseAdapter;
import com.rarnu.tools.root.common.DataappInfo;

import java.util.List;

public class ListUtils {

    private static List<DataappInfo> listOperate = null;

    public static List<DataappInfo> getOperateList() {
        return listOperate;
    }

    public static void setOperateList(List<DataappInfo> list) {
        listOperate = list;
    }

    public static int getListViewSelectedCount(List<DataappInfo> list) {
        int count = 0;

        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).checked) {
                count++;
            }
        }
        return count;
    }

    public static void setListViewItemSelectedStatus(List<DataappInfo> list, BaseAdapter adapter, Handler h, boolean selected) {
        for (int i = 0; i < list.size(); i++) {
            list.get(i).checked = selected;
        }
        adapter.notifyDataSetChanged();
        h.sendEmptyMessage(1);
    }
}
