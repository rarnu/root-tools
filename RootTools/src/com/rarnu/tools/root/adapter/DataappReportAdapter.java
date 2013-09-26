package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.holder.DataappReportAdapterHolder;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.utils.DrawableUtils;

import java.util.List;

public class DataappReportAdapter extends BaseAdapter<DataappInfo> {

    public DataappReportAdapter(Context context, List<DataappInfo> list) {
        super(context, list);
    }

    @Override
    public View getView(final int position, View convertView, ViewGroup parent) {

        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.dataapp_report_item, parent, false);
        }
        DataappReportAdapterHolder holder = (DataappReportAdapterHolder) v
                .getTag();
        if (holder == null) {
            holder = new DataappReportAdapterHolder();
            holder.icon = (ImageView) v.findViewById(R.id.report_icon);
            holder.name = (TextView) v.findViewById(R.id.report_name);
            holder.state = (TextView) v.findViewById(R.id.report_state);
            v.setTag(holder);
        }
        final DataappInfo item = list.get(position);
        if (item != null) {

            if (item.type == 1) {
                holder.icon.setBackgroundDrawable(GlobalInstance.pm.getApplicationIcon(item.info));
                holder.name.setText(GlobalInstance.pm.getApplicationLabel(item.info));
                switch (item.logId) {
                    case 0:
                        holder.state.setText(R.string.rep_bak_succ);
                        holder.state.setTextColor(DrawableUtils.getTextColorSecondary(context));
                        break;
                    case 1:
                        holder.state.setText(R.string.rep_bak_na);
                        holder.state.setTextColor(DrawableUtils.getTextColorSecondary(context));
                        break;
                    case 2:
                        holder.state.setText(R.string.rep_bak_fail);
                        holder.state.setTextColor(Color.RED);
                        break;
                }

            } else {
                holder.icon.setBackgroundDrawable(ApkUtils.getIconFromPackage(v.getContext(), item.info, GlobalInstance.backupPath));
                holder.name.setText(ApkUtils.getLabelFromPackage(v.getContext(), item.info, GlobalInstance.backupPath));
                switch (item.logId) {
                    case 0:
                        holder.state.setText(R.string.rep_res_succ);
                        holder.state.setTextColor(DrawableUtils.getTextColorSecondary(context));
                        break;
                    case 2:
                        holder.state.setText(R.string.rep_res_fail);
                        holder.state.setTextColor(Color.RED);
                        break;
                }
            }

        }

        return v;
    }

    @Override
    public String getValueText(DataappInfo item) {
        return "";
    }

}
