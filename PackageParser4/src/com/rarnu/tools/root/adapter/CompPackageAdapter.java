package com.rarnu.tools.root.adapter;

import java.util.ArrayList;
import java.util.List;

import android.content.pm.PackageInfo;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Filter;
import android.widget.Filterable;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.holder.CompPackageAdapterHolder;

public class CompPackageAdapter extends BaseAdapter implements Filterable {

	// [region] field define
	private LayoutInflater inflater;
	private List<PackageInfo> list;
	private List<PackageInfo> listFull;
	private ArrayFilter filter;

	private final Object lock = new Object();

	// [/region]

	// [region] constructor
	public CompPackageAdapter(LayoutInflater inflater, List<PackageInfo> list) {

		this.inflater = inflater;
		this.listFull = list;
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

		PackageInfo item = list.get(position);

		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.comp_package_item, parent, false);
		} else {
			v = convertView;
		}
		CompPackageAdapterHolder holder = (CompPackageAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new CompPackageAdapterHolder();
			holder.itemIcon = (ImageView) v.findViewById(R.id.itemIcon);
			holder.itemName = (TextView) v.findViewById(R.id.itemName);
			holder.tvReceiverCountValue = (TextView) v
					.findViewById(R.id.tvReceiverCountValue);
			v.setTag(holder);
		}

		if (item != null) {

			holder.itemIcon.setBackgroundDrawable(GlobalInstance.pm
					.getApplicationIcon(item.applicationInfo));
			holder.itemName.setText(GlobalInstance.pm
					.getApplicationLabel(item.applicationInfo));
			holder.itemName.setTextColor(item.applicationInfo.sourceDir
					.contains("/system/app/") ? Color.RED : Color.BLACK);
			holder.tvReceiverCountValue.setText(item.applicationInfo.sourceDir);

		}

		return v;
	}

	// [/region]

	// [region] filter
	@Override
	public Filter getFilter() {
		if (filter == null) {
			filter = new ArrayFilter();
		}
		return filter;
	}

	private class ArrayFilter extends Filter {

		@Override
		protected FilterResults performFiltering(CharSequence prefix) {
			list = listFull;
			FilterResults results = new FilterResults();
			if (prefix == null || prefix.length() == 0) {
				synchronized (lock) {
					ArrayList<PackageInfo> l = new ArrayList<PackageInfo>(list);
					results.values = l;
					results.count = l.size();
				}
			} else {

				String prefixString = prefix.toString().toLowerCase();

				final ArrayList<PackageInfo> values = new ArrayList<PackageInfo>(
						list);

				final int count = values.size();

				final ArrayList<PackageInfo> newValues = new ArrayList<PackageInfo>(
						count);

				for (int i = 0; i < count; i++) {
					final PackageInfo value = values.get(i);
					final String valueText = GlobalInstance.pm
							.getApplicationLabel(value.applicationInfo)
							+ value.packageName;

					if (valueText.indexOf(prefixString) != -1) {
						newValues.add(value);
					}

				}

				results.values = newValues;
				results.count = newValues.size();
			}

			return results;
		}

		@SuppressWarnings("unchecked")
		@Override
		protected void publishResults(CharSequence constraint,
				FilterResults results) {
			list = (List<PackageInfo>) results.values;
			if (results.count > 0) {
				notifyDataSetChanged();
			} else {
				notifyDataSetInvalidated();
			}
		}

	}
	// [/region]
}
