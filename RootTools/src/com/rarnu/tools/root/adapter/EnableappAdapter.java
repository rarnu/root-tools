package com.rarnu.tools.root.adapter;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.holder.EnableappAdapterHolder;

public class EnableappAdapter extends BaseAdapter<EnableappInfo> {

	public EnableappAdapter(Context context, List<EnableappInfo> list) {
		super(context, list);
	}

	@Override
	public View getView(final int position, View convertView, ViewGroup parent) {
		final EnableappInfo item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.enableapp_item, parent, false);
		} else {
			v = convertView;
		}
		EnableappAdapterHolder holder = (EnableappAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new EnableappAdapterHolder();
			holder.icon = (ImageView) v.findViewById(R.id.item_icon);
			holder.name = (TextView) v.findViewById(R.id.item_name);
			holder.path = (TextView) v.findViewById(R.id.item_path);
			holder.tvEnabled = (TextView) v.findViewById(R.id.tvEnabled);

			v.setTag(holder);
		}

		if (item != null) {
			holder.icon.setBackgroundDrawable(GlobalInstance.pm
					.getApplicationIcon(item.info));
			holder.name.setText(GlobalInstance.pm
					.getApplicationLabel(item.info));
			holder.path.setText(item.info.dataDir);
			holder.tvEnabled.setText(item.enabled ? R.string.package_enabled
					: R.string.package_disabled);
			holder.tvEnabled.setTextColor(item.enabled ? Color.GREEN
					: Color.RED);
		}

		return v;
	}

	public void sort() {
		Collections.sort(listFull, comparator);
	}

	private Comparator<EnableappInfo> comparator = new Comparator<EnableappInfo>() {
		@Override
		public int compare(EnableappInfo obj1, EnableappInfo obj2) {
			return obj1.enabled.compareTo(obj2.enabled);
		}

	};

	// [/region]

	@Override
	public String getValueText(EnableappInfo item) {
		return GlobalInstance.pm.getApplicationLabel(item.info).toString()
				+ item.info.packageName;
	}
}
