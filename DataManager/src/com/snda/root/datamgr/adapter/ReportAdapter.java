package com.snda.root.datamgr.adapter;

import java.util.List;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.snda.root.datamgr.GlobalInstance;
import com.snda.root.datamgr.R;
import com.snda.root.datamgr.utils.ApkUtils;
import com.snda.root.datamgr.utils.AppInfo;

public class ReportAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<AppInfo> list;

	public ReportAdapter(LayoutInflater inflater, List<AppInfo> list) {
		this.inflater = inflater;
		this.list = list;
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
			v = inflater.inflate(R.layout.report_item, parent, false);
		} else {
			v = convertView;
		}
		ReportAdapterHolder holder = (ReportAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new ReportAdapterHolder();
			holder.icon = (ImageView) v.findViewById(R.id.report_icon);
			holder.name = (TextView) v.findViewById(R.id.report_name);
			holder.state = (TextView) v.findViewById(R.id.report_state);
			v.setTag(holder);
		}

		if (item != null) {

			if (item.type == 1) {
				holder.icon.setBackgroundDrawable(GlobalInstance.pm
						.getApplicationIcon(item.info));
				holder.name.setText(GlobalInstance.pm
						.getApplicationLabel(item.info));
				switch (item.logId) {
				case 0:
					holder.state.setText(R.string.rep_bak_succ);
					holder.state.setTextColor(Color.GREEN);
					break;
				case 1:
					holder.state.setText(R.string.rep_bak_na);
					holder.state.setTextColor(Color.YELLOW);
					break;
				case 2:
					holder.state.setText(R.string.rep_bak_fail);
					holder.state.setTextColor(Color.RED);
					break;
				}

			} else {
				holder.icon.setBackgroundDrawable(ApkUtils.getIconFromPackage(v
						.getContext(), item.info));
				holder.name.setText(ApkUtils.getLabelFromPackage(
						v.getContext(), item.info));
				switch (item.logId) {
				case 0:
					holder.state.setText(R.string.rep_res_succ);
					holder.state.setTextColor(Color.GREEN);
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

}
