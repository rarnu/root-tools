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
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.holder.HostsAdapterHolder;

import java.util.List;

public class HostsAdapter extends BaseAdapter<HostRecordInfo> {

    private Handler h;
    private boolean locked0 = false;
    private boolean showCheckbox = true;

    public HostsAdapter(Context context, List<HostRecordInfo> list, Handler h, boolean locked0, boolean showCheckbox) {
        super(context, list);
        this.h = h;
        this.locked0 = locked0;
        this.showCheckbox = showCheckbox;
    }

    @Override
    public View getView(final int position, View convertView, ViewGroup parent) {

        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.host_item, parent, false);
        }
        HostsAdapterHolder holder = (HostsAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new HostsAdapterHolder();
            holder.tvItemIP = (TextView) v.findViewById(R.id.tvItem_IP);
            holder.tvItemDomain = (TextView) v.findViewById(R.id.tvItem_Domain);
            holder.chkItemChecked = (CheckBox) v.findViewById(R.id.chkItem_Checked);
            holder.imgLocked = (ImageView) v.findViewById(R.id.imgLocked);
            v.setTag(holder);
        }
        final HostRecordInfo item = list.get(position);
        if (item != null) {
            holder.tvItemIP.setText(item.ip);
            holder.tvItemDomain.setText(item.domain);
            holder.chkItemChecked.setVisibility(showCheckbox ? View.VISIBLE : View.GONE);

            if (locked0) {
                if (!showCheckbox) {
                    holder.chkItemChecked.setVisibility(View.GONE);
                } else {
                    boolean needLock = (position == 0 && item.ip.equals("127.0.0.1") && item.domain.equals("localhost"));
                    holder.chkItemChecked.setVisibility(needLock ? View.GONE : View.VISIBLE);
                    holder.imgLocked.setVisibility(needLock ? View.VISIBLE : View.GONE);
                }
            }

            holder.chkItemChecked.setChecked(item.checked);
            holder.chkItemChecked.setOnClickListener(new OnClickListener() {

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
    public String getValueText(HostRecordInfo item) {
        return item.ip + item.domain;
    }
}
