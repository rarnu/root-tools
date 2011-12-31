package com.snda.root.bcm.adapter;

import java.util.List;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.snda.root.bcm.PackgeFullInfo;
import com.snda.root.bcm.R;

public class PackageAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<PackgeFullInfo> list;

	public PackageAdapter(LayoutInflater inflater, List<PackgeFullInfo> list) {

		this.inflater = inflater;
		this.list = list;
	}

	public int getCount() {
		return list.size();
	}

	public Object getItem(int position) {
		return list.get(position);
	}

	public long getItemId(int position) {
		return position;
	}

	public View getView(int position, View convertView, ViewGroup parent) {

		PackgeFullInfo item = list.get(position);

		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.package_item, parent, false);
		} else {
			v = convertView;
		}
		PackageHolder holder = (PackageHolder) v.getTag();
		if (holder == null) {
			holder = new PackageHolder();
			holder.itemIcon = (ImageView) v.findViewById(R.id.itemIcon);
			holder.itemName = (TextView) v.findViewById(R.id.itemName);
			holder.tvSystemApp = (TextView) v.findViewById(R.id.tvSystemApp);
			holder.tvReceiverCountValue = (TextView) v
					.findViewById(R.id.tvReceiverCountValue);
			v.setTag(holder);
		}

		if (item != null) {

			holder.itemIcon.setBackgroundDrawable(item.icon);
			holder.itemName.setText(item.label);
			holder.itemName.setTextColor(item.isSytemApp ? Color.BLUE
					: Color.BLACK);
			holder.tvSystemApp.setVisibility(item.isSytemApp ? View.VISIBLE
					: View.GONE);
			holder.tvReceiverCountValue.setText(String.format("C:%d/E:%d/D:%d",
					item.receiverCount, item.enabledReceiver,
					item.disabledReceiver));

		}

		return v;
	}

}
