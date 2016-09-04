package com.rarnu.tools.neo.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Switch;
import android.widget.TextView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseAdapter;
import com.rarnu.tools.neo.data.CompInfo;

import java.util.List;

public class CompDetailAdapter extends BaseAdapter<CompInfo> {

    public CompDetailAdapter(Context context, List<CompInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(CompInfo item) {
        return item.getCompName();
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.listitem_compdetail, parent, false);
        }
        CompHolder holder = (CompHolder) v.getTag();
        if (holder == null) {
            holder = new CompHolder(v, R.id.prefStatus, R.id.tvName);
            v.setTag(holder);
        }
        CompInfo item = list.get(position);
        holder.setItem(item);
        return v;
    }

    private class CompHolder {

        Switch prefStatus = null;
        TextView tvName = null;

        CompHolder(View v, int statusId, int nameId) {
            prefStatus = (Switch) v.findViewById(statusId);
            tvName = (TextView) v.findViewById(nameId);
        }

        void setItem(CompInfo item) {
            prefStatus.setChecked(item.enabled);
            tvName.setText(item.getCompName());
        }
    }
}
