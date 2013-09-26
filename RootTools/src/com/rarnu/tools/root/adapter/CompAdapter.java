package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.graphics.Color;
import android.text.Html;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.CompInfo;
import com.rarnu.tools.root.holder.CompAdapterHolder;

import java.util.List;

public class CompAdapter extends BaseAdapter<CompInfo> {

    public CompAdapter(Context context, List<CompInfo> list) {
        super(context, list);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.comp_receiver_item, parent, false);
        }
        CompAdapterHolder holder = (CompAdapterHolder) v.getTag();

        if (holder == null) {
            holder = new CompAdapterHolder();
            holder.itemReceiverName = (TextView) v.findViewById(R.id.itemReceiverName);
            holder.itemReceiverAction = (TextView) v.findViewById(R.id.itemReceiverAction);
            holder.itemReceiverStatus = (TextView) v.findViewById(R.id.itemReceiverStatus);
            v.setTag(holder);
        }

        CompInfo item = list.get(position);

        if (item != null) {
            String compName = item.getCompName();

            if (item.isActivity()) {
                compName += "<font color=\"#6495ED\">(R)</font>";
            } else {
                if (item.isServiceRunning(context)) {
                    compName = "<font color=\"red\">" + compName + "</font>";
                }
                compName += "<font color=\"#6495ED\">(S)</font>";
            }

            holder.itemReceiverName.setText(Html.fromHtml(compName));
            holder.itemReceiverStatus.setText(item.enabled ? R.string.comp_enabled : R.string.comp_disabled);
            holder.itemReceiverStatus.setTextColor(item.enabled ? Color.GREEN : Color.RED);
            String ret = "<font color=\"yellow\"><small>" + item.fullPackageName + "</small></font><br>";

            if (item.isActivity()) {
                ret = item.appendIntents(ret);
            }
            holder.itemReceiverAction.setText(Html.fromHtml(ret));
        }

        return v;
    }

    @Override
    public String getValueText(CompInfo item) {
        return "";
    }

}
