package com.snda.root.datamgr.adapter;

import java.util.List;

import android.os.Handler;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.BaseAdapter;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;

import com.snda.root.datamgr.GlobalInstance;
import com.snda.root.datamgr.R;
import com.snda.root.datamgr.utils.ApkUtils;
import com.snda.root.datamgr.utils.AppInfo;

public class AppAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<AppInfo> list;
	private Handler h;
	private int type;

	public AppAdapter(LayoutInflater inflater, List<AppInfo> list, Handler h,
			int type) {
		this.inflater = inflater;
		this.list = list;
		this.h = h;
		this.type = type;
	}

	public int getCount() {
		return list.size();
	}

	public Object getItem(int arg0) {
		return list.get(arg0);
	}

	public long getItemId(int arg0) {
		return arg0;
	}

	public View getView(final int position, View convertView, ViewGroup parent) {
		final AppInfo item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.app_item, parent, false);
		} else {
			v = convertView;
		}
		AppAdapterHolder holder = (AppAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new AppAdapterHolder();
			holder.icon = (ImageView) v.findViewById(R.id.item_icon);
			holder.name = (TextView) v.findViewById(R.id.item_name);
			holder.path = (TextView) v.findViewById(R.id.item_path);
			holder.select = (CheckBox) v.findViewById(R.id.chk_select);

			v.setTag(holder);
		}

		if (item != null) {
			holder.select.setChecked(item.checked);

			if (type == 1) {
				holder.icon.setBackgroundDrawable(GlobalInstance.pm
						.getApplicationIcon(item.info));
				holder.name.setText(GlobalInstance.pm
						.getApplicationLabel(item.info));
				holder.path.setText(item.info.dataDir);
			} else {
				holder.icon.setBackgroundDrawable(ApkUtils.getIconFromPackage(v
						.getContext(), item.info));
				holder.name.setText(ApkUtils.getLabelFromPackage(
						v.getContext(), item.info));
				holder.path.setText(item.info.packageName + ".apk");
			}

			holder.select.setOnClickListener(new OnClickListener() {

				@Override
				public void onClick(View v) {
					item.checked = ((CheckBox) v).isChecked();
					h.sendEmptyMessage(1);
				}
			});

		}

		return v;
	}

}
