package com.rarnu.tools.root.adapter;

import java.util.ArrayList;
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

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.holder.SysappAdapterHolder;

public class SysappAdapter extends BaseAdapter implements Filterable {

	// [region] field define
	private LayoutInflater inflater;
	private List<SysappInfo> listFull;
	private List<SysappInfo> list;
	private ArrayFilter filter;

	// [/region]

	// [region] constructor
	public SysappAdapter(LayoutInflater inflater, List<SysappInfo> list) {
		this.inflater = inflater;
		this.listFull = list;
		this.list = list;
	}

	// [/region]

	// [region] business logic
	public void setNewList(List<SysappInfo> list) {
		this.listFull = list;
		this.list = list;
		this.notifyDataSetChanged();
	}

	public void deleteItem(SysappInfo item) {
		this.list.remove(item);
		this.listFull.remove(item);
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
	public View getView(int position, View convertView, ViewGroup parent) {
		SysappInfo item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.sysapp_item, parent, false);
		} else {
			v = convertView;
		}
		SysappAdapterHolder holder = (SysappAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new SysappAdapterHolder();
			holder.icon = (ImageView) v.findViewById(R.id.item_icon);
			holder.name = (TextView) v.findViewById(R.id.item_name);
			holder.path = (TextView) v.findViewById(R.id.item_path);
			v.setTag(holder);
		}

		if (item != null) {
			holder.icon.setBackgroundDrawable(GlobalInstance.pm.getApplicationIcon(item.info));
			holder.name.setText(GlobalInstance.pm.getApplicationLabel(item.info));
			holder.path.setText(item.info.sourceDir);

			holder.name.setTextColor(Color.BLACK);

			switch (item.level) {
			case 0:
				holder.name.setTextColor(Color.RED);
				break;
			case 1:
				holder.name.setTextColor(Color.GREEN);
				break;
			case 2:
				holder.name.setTextColor(Color.BLUE);
				break;
			}

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
				ArrayList<SysappInfo> l = new ArrayList<SysappInfo>(list);
				results.values = l;
				results.count = l.size();
			} else {

				String prefixString = prefix.toString().toLowerCase();

				final ArrayList<SysappInfo> values = new ArrayList<SysappInfo>(list);

				final int count = values.size();

				final ArrayList<SysappInfo> newValues = new ArrayList<SysappInfo>(count);

				for (int i = 0; i < count; i++) {
					final SysappInfo value = values.get(i);
					final String valueText = GlobalInstance.pm.getApplicationLabel(value.info).toString()
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
		protected void publishResults(CharSequence constraint, FilterResults results) {
			list = (List<SysappInfo>) results.values;
			if (results.count > 0) {
				notifyDataSetChanged();
			} else {
				notifyDataSetInvalidated();
			}
		}

	}
	// [/region]
}
