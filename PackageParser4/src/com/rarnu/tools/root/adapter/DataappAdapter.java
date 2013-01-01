package com.rarnu.tools.root.adapter;

import java.util.ArrayList;
import java.util.List;

import android.os.Handler;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.CheckBox;
import android.widget.Filter;
import android.widget.Filterable;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.holder.DataappAdapterHolder;
import com.rarnu.tools.root.utils.ApkUtils;

public class DataappAdapter extends BaseAdapter implements Filterable {

	// [region] field define
	private LayoutInflater inflater;
	private List<DataappInfo> list;
	private List<DataappInfo> listFull;
	private Handler h;
	private int type;
	private ArrayFilter filter;

	private final Object lock = new Object();

	// [/region]

	// [region] constructor
	public DataappAdapter(LayoutInflater inflater, List<DataappInfo> list,
			Handler h, int type) {
		this.inflater = inflater;
		this.listFull = list;
		this.list = list;
		this.h = h;
		this.type = type;
	}

	// [/region]

	// [region] business logic
	public void deleteItem(DataappInfo item) {
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
		final DataappInfo item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.dataapp_item, parent, false);
		} else {
			v = convertView;
		}
		DataappAdapterHolder holder = (DataappAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new DataappAdapterHolder();
			holder.icon = (ImageView) v.findViewById(R.id.item_icon);
			holder.name = (TextView) v.findViewById(R.id.item_name);
			holder.path = (TextView) v.findViewById(R.id.item_path);
			holder.select = (CheckBox) v.findViewById(R.id.chk_select);

			v.setTag(holder);
		}

		if (item != null) {
			holder.select.setChecked(item.checked);

			if (type == 1) {
				holder.icon.setBackgroundDrawable(GlobalInstance.pm
						.getApplicationIcon(item.info));
				holder.name.setText(GlobalInstance.pm
						.getApplicationLabel(item.info));
				holder.path.setText(item.info.dataDir);
			} else {
				holder.icon.setBackgroundDrawable(ApkUtils.getIconFromPackage(
						v.getContext(), item.info));
				holder.name.setText(ApkUtils.getLabelFromPackage(
						v.getContext(), item.info));
				holder.path.setText(item.info.packageName + ".apk");
			}

			holder.select.setOnClickListener(new OnClickListener() {

				@Override
				public void onClick(View v) {
					item.checked = ((CheckBox) v).isChecked();
					h.sendEmptyMessage(1);
				}
			});

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
					ArrayList<DataappInfo> l = new ArrayList<DataappInfo>(list);
					results.values = l;
					results.count = l.size();
				}
			} else {

				String prefixString = prefix.toString().toLowerCase();

				final ArrayList<DataappInfo> values = new ArrayList<DataappInfo>(
						list);

				final int count = values.size();

				final ArrayList<DataappInfo> newValues = new ArrayList<DataappInfo>(
						count);

				for (int i = 0; i < count; i++) {
					final DataappInfo value = values.get(i);
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
			list = (List<DataappInfo>) results.values;
			if (results.count > 0) {
				notifyDataSetChanged();
			} else {
				notifyDataSetInvalidated();
			}
		}

	}
	// [/region]
}
