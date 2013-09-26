package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.os.Handler;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MemIgnoreInfo;
import com.rarnu.tools.root.holder.MemIgnoreAdapterHolder;

import java.util.List;

public class MemIgnoreAdapter extends BaseAdapter<MemIgnoreInfo> {

    private Handler h;

    public MemIgnoreAdapter(Context context, List<MemIgnoreInfo> list, Handler h) {
        super(context, list);
        this.h = h;
    }

    @Override
    public View getView(final int position, View convertView, ViewGroup parent) {


        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.mem_ignore_item, parent, false);
        }
        MemIgnoreAdapterHolder holder = (MemIgnoreAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new MemIgnoreAdapterHolder();
            holder.tvNamespace = (TextView) v.findViewById(R.id.tvNamespace);
            holder.chkChecked = (CheckBox) v.findViewById(R.id.chkChecked);
            holder.imgLocked = (ImageView) v.findViewById(R.id.imgLocked);
            v.setTag(holder);
        }
        final MemIgnoreInfo item = list.get(position);
        if (item != null) {
            holder.tvNamespace.setText(item.namespace);
            holder.imgLocked.setVisibility(item.locked ? View.VISIBLE : View.GONE);
            holder.chkChecked.setVisibility(item.locked ? View.GONE : View.VISIBLE);
            holder.chkChecked.setChecked(item.checked);
            holder.chkChecked.setOnClickListener(new OnClickListener() {
                @Override
                public void onClick(View v) {
                    item.checked = ((CheckBox) v).isChecked();
                    h.sendEmptyMessage(1);
                }
            });
        }
        return v;
    }

    @Override
    public String getValueText(MemIgnoreInfo item) {
        return "";
    }
}
