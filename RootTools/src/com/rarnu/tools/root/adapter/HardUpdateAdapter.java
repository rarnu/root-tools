package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.holder.HardUpdateAdapterHolder;
import com.rarnu.tools.root.utils.ApkUtils;

import java.util.List;

public class HardUpdateAdapter extends BaseAdapter<DataappInfo> {

    private static int[] apkStat = new int[]{
            R.string.apkstat_0, R.string.apkstat_1, R.string.apkstat_2, R.string.apkstat_3, R.string.apkstat_4
    };
    private Handler hButtonClick;
    private boolean enabledButtons;

    public HardUpdateAdapter(Context context, List<DataappInfo> list, Handler h) {
        super(context, list);
        this.hButtonClick = h;
        enabledButtons = false;
    }

    public void setEnableButtons(boolean enabled) {
        this.enabledButtons = enabled;
        this.notifyDataSetChanged();
    }

    @Override
    public View getView(final int position, View convertView, ViewGroup parent) {

        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.hardupdate_item, parent, false);
        }
        HardUpdateAdapterHolder holder = (HardUpdateAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new HardUpdateAdapterHolder();
            holder.icon = (ImageView) v.findViewById(R.id.item_icon);
            holder.name = (TextView) v.findViewById(R.id.item_name);
            holder.path = (TextView) v.findViewById(R.id.item_path);
            holder.update = (Button) v.findViewById(R.id.btn_update);

            v.setTag(holder);
        }
        final DataappInfo item = list.get(position);
        if (item != null) {
            holder.icon.setBackgroundDrawable(ApkUtils.getIconFromPackageFile(v.getContext(), item.info, item.localPath));
            holder.name.setText(ApkUtils.getLabelFromPackageFile(v.getContext(), item.info, item.localPath));
            holder.path.setText(apkStat[item.apkStatus]);
            switch (item.apkStatus) {
                case 0:
                case 1:
                    holder.update.setText(R.string.btn_update);
                    holder.update.setVisibility(View.VISIBLE);
                    break;
                case 2:
                    holder.update.setVisibility(View.GONE);
                    break;
                case 3:
                    holder.update.setText(R.string.btn_update_install);
                    holder.update.setVisibility(View.VISIBLE);
                    break;
                case 4:
                    holder.name.setText(item.localPath);
                    holder.update.setVisibility(View.GONE);
                    break;
            }

            if (!enabledButtons) {
                holder.update.setEnabled(false);
            } else {

                if (item.installing) {
                    holder.update.setOnClickListener(null);
                    holder.update.setEnabled(false);
                } else {
                    holder.update.setEnabled(true);
                    holder.update.setOnClickListener(new OnClickListener() {

                        @Override
                        public void onClick(View v) {
                            Message msg = new Message();
                            msg.what = 1;
                            msg.arg1 = position;
                            msg.obj = item;
                            hButtonClick.sendMessage(msg);

                        }
                    });
                }
            }
        }

        return v;
    }

    @Override
    public String getValueText(DataappInfo item) {
        return GlobalInstance.pm.getApplicationLabel(item.info).toString() + item.info.packageName;
    }
}
