package com.rarnu.tools.root.adapter;

import java.util.ArrayList;
import java.util.List;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Filter;
import android.widget.Filterable;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.holder.MemProcessAdapterHolder;

public class MemProcessAdapter extends BaseAdapter implements Filterable {

	// [region] field define
	private List<MemProcessInfo> list;
	private List<MemProcessInfo> listFull;
	private LayoutInflater inflater;
	private ArrayFilter filter;

	private final Object lock = new Object();

	// [/region]

	// [region] constructor

	public MemProcessAdapter(List<MemProcessInfo> list, LayoutInflater inflater) {
		this.listFull = list;
		this.list = list;
		this.inflater = inflater;
	}

	// [/region]

	// [region] business logic
	public void deleteItem(MemProcessInfo item) {
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
	public Object getItem(int position) {
		return list.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		MemProcessInfo item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.mem_process_item, parent, false);
		} else {
			v = convertView;
		}
		MemProcessAdapterHolder holder = (MemProcessAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new MemProcessAdapterHolder();
			holder.item_icon = (ImageView) v.findViewById(R.id.item_icon);
			holder.item_name = (TextView) v.findViewById(R.id.item_name);
			holder.item_memory_used = (TextView) v
					.findViewById(R.id.item_memory_used);
			holder.item_namespace = (TextView) v
					.findViewById(R.id.item_namespace);
			v.setTag(holder);
		}

		if (item != null) {
			if (item.appInfo == null) {
				holder.item_icon.setBackgroundDrawable(v.getResources()
						.getDrawable(R.drawable.android));
				holder.item_name.setText(item.NAME);

				holder.item_namespace.setText("");
			} else {
				holder.item_icon.setBackgroundDrawable(GlobalInstance.pm
						.getApplicationIcon(item.appInfo));
				holder.item_name.setText(GlobalInstance.pm
						.getApplicationLabel(item.appInfo));
				holder.item_namespace.setText(item.NAME);
			}
			holder.item_memory_used.setText(String.format("%dM", item.RSS));
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
					ArrayList<MemProcessInfo> l = new ArrayList<MemProcessInfo>(
							list);
					results.values = l;
					results.count = l.size();
				}
			} else {

				String prefixString = prefix.toString().toLowerCase();

				final ArrayList<MemProcessInfo> values = new ArrayList<MemProcessInfo>(
						list);

				final int count = values.size();

				final ArrayList<MemProcessInfo> newValues = new ArrayList<MemProcessInfo>(
						count);

				for (int i = 0; i < count; i++) {
					final MemProcessInfo value = values.get(i);
					String valueText = value.NAME;
					if (value.appInfo != null) {
						valueText += GlobalInstance.pm
								.getApplicationLabel(value.appInfo);
					}

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
			list = (List<MemProcessInfo>) results.values;
			if (results.count > 0) {
				notifyDataSetChanged();
			} else {
				notifyDataSetInvalidated();
			}
		}

	}
	// [/region]
}
