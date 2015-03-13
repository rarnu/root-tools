package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.BuildPropInfo;
import com.rarnu.tools.root.holder.BuildPropAdapterHolder;

import java.util.List;

public class BuildPropAdapter extends BaseAdapter<BuildPropInfo> {

    public BuildPropAdapter(Context context, List<BuildPropInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(BuildPropInfo item) {
        return item.buildName + item.buildValue;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.build_prop_item, parent, false);
        }
        BuildPropAdapterHolder holder = (BuildPropAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new BuildPropAdapterHolder();
            holder.tvPropName = (TextView) v.findViewById(R.id.tvPropName);
            holder.tvPropValue = (TextView) v.findViewById(R.id.tvPropValue);
            v.setTag(holder);
        }
        final BuildPropInfo item = list.get(position);
        if (item != null) {
            holder.tvPropName.setText(item.buildName);
            holder.tvPropValue.setText(item.buildValue);
        }
        return v;
    }
}
