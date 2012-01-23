package com.snda.root.bcm.adapter;

import java.util.List;

import android.content.pm.PackageParser;
import android.graphics.Color;
import android.text.Html;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.snda.root.bcm.R;
import com.snda.root.bcm.ComponentFullInfo;

public class ComponentAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<ComponentFullInfo> list;

	public ComponentAdapter(LayoutInflater inflater,
			List<ComponentFullInfo> list) {
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
		ComponentFullInfo item = list.get(position);

		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.receiver_item, parent, false);
		} else {
			v = convertView;
		}

		ComponentHolder holder = (ComponentHolder) v.getTag();

		if (holder == null) {
			holder = new ComponentHolder();
			holder.itemReceiverName = (TextView) v
					.findViewById(R.id.itemReceiverName);
			holder.itemReceiverAction = (TextView) v
					.findViewById(R.id.itemReceiverAction);
			holder.itemReceiverStatus = (TextView) v
					.findViewById(R.id.itemReceiverStatus);

			v.setTag(holder);
		}

		if (item != null) {

			String compName = item.component.className
					.substring(item.component.className.lastIndexOf(".") + 1);

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

}
