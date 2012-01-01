package com.snda.root.bcm.adapter;

import java.util.List;

import android.content.pm.PackageParser;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.snda.root.bcm.R;
import com.snda.root.bcm.ReceiverFullInfo;

public class ReceiverAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<ReceiverFullInfo> list;

	public ReceiverAdapter(LayoutInflater inflater, List<ReceiverFullInfo> list) {
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
		ReceiverFullInfo item = list.get(position);

		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.receiver_item, parent, false);
		} else {
			v = convertView;
		}

		ReceiverHolder holder = (ReceiverHolder) v.getTag();

		if (holder == null) {
			holder = new ReceiverHolder();
			holder.itemReceiverName = (TextView) v
					.findViewById(R.id.itemReceiverName);
			holder.itemReceiverAction = (TextView) v
					.findViewById(R.id.itemReceiverAction);
			holder.itemReceiverStatus = (TextView) v
					.findViewById(R.id.itemReceiverStatus);

			v.setTag(holder);
		}

		if (item != null) {
//			holder.itemReceiverName.setText(item.receiver.info.name
//					.substring(item.receiver.info.name.lastIndexOf(".") + 1));
			holder.itemReceiverName.setText(item.receiver.info.name);
			holder.itemReceiverStatus
					.setText(item.enabled ? R.string.comp_enabled
							: R.string.comp_disabled);
			holder.itemReceiverStatus.setTextColor(item.enabled ? Color.GREEN
					: Color.RED);
			String ret = "";
			int i = 0;
			if (item.receiver.intents != null) {
				if (item.receiver.intents.size() > 0) {
					for (PackageParser.ActivityIntentInfo aii : item.receiver.intents) {
						if (aii.countActions() > 0) {
							for (i = 0; i < aii.countActions(); i++) {
								ret += aii.getAction(i) + "\n";
							}
						}
					}
				}
			}
			holder.itemReceiverAction.setText(ret);
		}

		return v;
	}

}
