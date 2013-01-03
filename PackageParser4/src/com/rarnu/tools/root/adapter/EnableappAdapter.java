package com.rarnu.tools.root.adapter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

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
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.holder.EnableappAdapterHolder;

public class EnableappAdapter extends BaseAdapter implements Filterable {

	// [region] field define
	private LayoutInflater inflater;
	private List<EnableappInfo> list;
	private List<EnableappInfo> listFull;
	private ArrayFilter filter;

	private final Object lock = new Object();

	// [/region]

	// [region] constructor
	public EnableappAdapter(LayoutInflater inflater, List<EnableappInfo> list) {
		this.inflater = inflater;
		this.listFull = list;
		this.list = list;
	}

	// [/region]

	// [region] business logic
	public void setNewList(List<EnableappInfo> list) {
		this.listFull = list;
		this.list = list;
		this.notifyDataSetChanged();
	}
	
	public void deleteItem(EnableappInfo item) {
		list.remove(item);
		listFull.remove(item);
		notifyDataSetChanged();
	}

	// [/region]

	// [region] adapter
	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public Object getItem(int arg0) {
		return list.get(arg0);
	}

	@Override
	public long getItemId(int arg0) {
		return arg0;
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
					ArrayList<EnableappInfo> l = new ArrayList<EnableappInfo>(
							list);
					results.values = l;
					results.count = l.size();
				}
			} else {

				String prefixString = prefix.toString().toLowerCase();

				final ArrayList<EnableappInfo> values = new ArrayList<EnableappInfo>(
						list);

				final int count = values.size();

				final ArrayList<EnableappInfo> newValues = new ArrayList<EnableappInfo>(
						count);

				for (int i = 0; i < count; i++) {
					final EnableappInfo value = values.get(i);
					final String valueText = GlobalInstance.pm
							.getApplicationLabel(value.info).toString()
							+ value.info.packageName;

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
			list = (List<EnableappInfo>) results.values;
			if (results.count > 0) {
				notifyDataSetChanged();
			} else {
				notifyDataSetInvalidated();
			}
		}

	}

	// [/region]

	// [region] sort

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
}
