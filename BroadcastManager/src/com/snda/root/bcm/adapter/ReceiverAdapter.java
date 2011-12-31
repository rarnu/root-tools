package com.snda.root.bcm.adapter;

import java.util.List;

import android.content.pm.PackageParser;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.snda.root.bcm.R;

public class ReceiverAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<PackageParser.Activity> list;

	public ReceiverAdapter(LayoutInflater inflater,
			List<PackageParser.Activity> list) {
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
		PackageParser.Activity item = list.get(position);

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

			v.setTag(holder);
		}

		if (item != null) {
			holder.itemReceiverName.setText(item.info.name
					.substring(item.info.name.lastIndexOf(".")+1));
			String ret = "";
			int i = 0;
			if (item.intents != null) {
				if (item.intents.size() > 0) {
					for (PackageParser.ActivityIntentInfo aii : item.intents) {
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
