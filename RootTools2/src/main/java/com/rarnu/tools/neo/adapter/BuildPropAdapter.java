package com.rarnu.tools.neo.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseAdapter;
import com.rarnu.tools.neo.data.BuildPropInfo;

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
            v = inflater.inflate(R.layout.listitem_buildprop, parent, false);
        }
        BuildPropAdapterHolder holder = (BuildPropAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new BuildPropAdapterHolder(v, R.id.tvPropName, R.id.tvPropValue);
            v.setTag(holder);
        }
        BuildPropInfo item = list.get(position);
        holder.setItem(item);
        return v;
    }

    private class BuildPropAdapterHolder {
        public TextView tvPropName;
        public TextView tvPropValue;

        BuildPropAdapterHolder(View v, int nameId, int valueId) {
            tvPropName = (TextView) v.findViewById(nameId);
            tvPropValue = (TextView) v.findViewById(valueId);
        }

        void setItem(BuildPropInfo item) {
            tvPropName.setText(item.buildName);
            tvPropValue.setText(item.buildValue);
        }
    }

}
