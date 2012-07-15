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
import com.rarnu.tools.root.common.CacheInfo;
import com.rarnu.tools.root.holder.CacheAdapterHolder;

public class CacheAdapter extends BaseAdapter implements Filterable {

	// [region] field define
	private LayoutInflater inflater;
	private List<CacheInfo> list;
	private List<CacheInfo> listFull;
	private ArrayFilter filter;

	// [/region]

	// [region] constructor
	public CacheAdapter(LayoutInflater inflater, List<CacheInfo> list) {
		this.inflater = inflater;
		this.list = list;
		this.listFull = list;
	}

	// [/region]

	// [region] business logic
	public void deleteItem(CacheInfo item) {
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
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.cache_item, parent, false);
		}
		CacheAdapterHolder holder = (CacheAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new CacheAdapterHolder();
			holder.imgIcon = (ImageView) v.findViewById(R.id.item_icon);
			holder.tvName = (TextView) v.findViewById(R.id.item_name);
			holder.tvPath = (TextView) v.findViewById(R.id.item_path);
			holder.tvCache = (TextView) v.findViewById(R.id.item_cache);
			v.setTag(holder);
		}
		CacheInfo item = list.get(position);
		if (item != null) {
			holder.imgIcon.setBackgroundDrawable(GlobalInstance.pm.getApplicationIcon(item.info.applicationInfo));
			holder.tvName.setText(GlobalInstance.pm.getApplicationLabel(item.info.applicationInfo));
			holder.tvPath.setText(item.info.applicationInfo.sourceDir);
			holder.tvCache.setText(item.cacheSize);
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
				ArrayList<CacheInfo> l = new ArrayList<CacheInfo>(list);
				results.values = l;
				results.count = l.size();
			} else {

				String prefixString = prefix.toString().toLowerCase();

				final ArrayList<CacheInfo> values = new ArrayList<CacheInfo>(list);

				final int count = values.size();

				final ArrayList<CacheInfo> newValues = new ArrayList<CacheInfo>(count);

				for (int i = 0; i < count; i++) {
					final CacheInfo value = values.get(i);
					final String valueText = GlobalInstance.pm.getApplicationLabel(value.info.applicationInfo)
							.toString() + value.info.packageName;

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
		protected void publishResults(CharSequence constraint, FilterResults results) {
			list = (List<CacheInfo>) results.values;
			if (results.count > 0) {
				notifyDataSetChanged();
			} else {
				notifyDataSetInvalidated();
			}
		}

	}
	// [/region]
}
