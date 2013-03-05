package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.BusyboxInfo;
import com.rarnu.tools.root.holder.BusyboxAdapterHolder;

public class BusyboxAdapter extends BaseAdapter<BusyboxInfo> {

	public BusyboxAdapter(Context context, List<BusyboxInfo> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.busybox_item, parent, false);
		}
		BusyboxAdapterHolder holder = (BusyboxAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new BusyboxAdapterHolder();
			holder.tvTitle = (TextView) v.findViewById(R.id.tvTitle);
			holder.imgState = (ImageView) v.findViewById(R.id.imgState);
			v.setTag(holder);
		}
		BusyboxInfo item = list.get(position);
		if (item != null) {
			holder.tvTitle.setText(item.title);
			switch (item.state) {
			case BusyboxInfo.STATE_BANNED:
				holder.imgState.setImageResource(R.drawable.banned);
				break;
			case BusyboxInfo.STATE_WARNING:
				holder.imgState.setImageResource(R.drawable.warning);
				break;
			case BusyboxInfo.STATE_NORMAL:
				holder.imgState.setImageResource(R.drawable.ok);
				break;
			}
		}
		return v;
	}

	@Override
	public String getValueText(BusyboxInfo item) {
		return "";
	}

}
