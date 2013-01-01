package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.pm.PackageParser;
import android.graphics.Color;
import android.text.Html;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.common.CompInfo;
import com.rarnu.tools.root.holder.CompAdapterHolder;

public class CompAdapter extends BaseAdapter {

	// [region] field define
	private LayoutInflater inflater;
	private List<CompInfo> list;

	// [/region]

	// [region] constructor
	public CompAdapter(LayoutInflater inflater, List<CompInfo> list) {
		this.inflater = inflater;
		this.list = list;
	}

	// [/region]

	// [region] adapter
	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public Object getItem(int position) {
		return list.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		CompInfo item = list.get(position);

		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.comp_receiver_item, parent, false);
		} else {
			v = convertView;
		}

		CompAdapterHolder holder = (CompAdapterHolder) v.getTag();

		if (holder == null) {
			holder = new CompAdapterHolder();
			holder.itemReceiverName = (TextView) v
					.findViewById(R.id.itemReceiverName);
			holder.itemReceiverAction = (TextView) v
					.findViewById(R.id.itemReceiverAction);
			holder.itemReceiverStatus = (TextView) v
					.findViewById(R.id.itemReceiverStatus);

			v.setTag(holder);
		}

		if (item != null) {

			String compName = ((PackageParser.Component<?>) item.component).className
					.substring(((PackageParser.Component<?>) item.component).className
							.lastIndexOf(".") + 1);

			if (item.component instanceof PackageParser.Activity) {
				compName += "<font color=\"blue\">(R)</font>";
			} else {
				compName += "<font color=\"blue\">(S)</font>";
			}

			holder.itemReceiverName.setText(Html.fromHtml(compName));
			holder.itemReceiverStatus
					.setText(item.enabled ? R.string.comp_enabled
							: R.string.comp_disabled);
			holder.itemReceiverStatus.setTextColor(item.enabled ? 0xFF008000
					: Color.RED);
			String ret = "";
			int i = 0;
			// Integer act;
			if (item.component instanceof PackageParser.Activity) {
				PackageParser.Activity pa = (PackageParser.Activity) item.component;
				if (pa.intents != null) {
					if (pa.intents.size() > 0) {
						for (PackageParser.ActivityIntentInfo aii : pa.intents) {
							if (aii.countActions() > 0) {
								for (i = 0; i < aii.countActions(); i++) {
									ret += aii
											.getAction(i)
											.substring(
													aii.getAction(i)
															.lastIndexOf(".") + 1)
											.replace("_", " ").toLowerCase()
											+ "\n";
								}
							}
						}
					}
				}
			}
			holder.itemReceiverAction.setText(ret);
		}

		return v;
	}

	// [/region]
}
