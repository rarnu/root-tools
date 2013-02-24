package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.content.pm.PackageParser;
import android.graphics.Color;
import android.text.Html;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.common.CompInfo;
import com.rarnu.tools.root.holder.CompAdapterHolder;

public class CompAdapter extends InnerAdapter<CompInfo> {

	public CompAdapter(Context context, List<CompInfo> list) {
		super(context, list);
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
				compName += "<font color=\"#6495ED\">(R)</font>";
			} else {
				compName += "<font color=\"#6495ED\">(S)</font>";
			}

			holder.itemReceiverName.setText(Html.fromHtml(compName));
			holder.itemReceiverStatus
					.setText(item.enabled ? R.string.comp_enabled
							: R.string.comp_disabled);
			holder.itemReceiverStatus.setTextColor(item.enabled ? Color.GREEN
					: Color.RED);
			String ret = "<font color=\"yellow\"><small>" + item.fullPackageName
					+ "</small></font><br>";
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
											+ "<br />";
								}
							}
						}
					}
				}
			}
			holder.itemReceiverAction.setText(Html.fromHtml(ret));
		}

		return v;
	}

	@Override
	public String getValueText(CompInfo item) {
		return "";
	}

}
