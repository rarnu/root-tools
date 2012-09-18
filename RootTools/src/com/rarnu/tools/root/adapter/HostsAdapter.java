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

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.holder.HostsAdapterHolder;

public class HostsAdapter extends BaseAdapter implements Filterable {

	// [region] field define
	private LayoutInflater inflater;
	private List<HostRecordInfo> listFull;
	private List<HostRecordInfo> list;
	private Handler h;
	private ArrayFilter filter;
	private boolean locked0 = false;
	private boolean showCheckbox = true;

	private final Object lock = new Object();

	// [/region]

	// [region] constructor
	public HostsAdapter(LayoutInflater inflater, List<HostRecordInfo> list,
			Handler h, boolean locked0, boolean showCheckbox) {
		this.inflater = inflater;
		this.listFull = list;
		this.list = list;
		this.h = h;
		this.locked0 = locked0;
		this.showCheckbox = showCheckbox;
	}

	// [/region]

	// [region] business logic
	public void deleteItem(List<HostRecordInfo> items) {
		for (HostRecordInfo info : items) {
			list.remove(info);
			listFull.remove(info);
		}
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
	public View getView(final int position, View convertView, ViewGroup parent) {
		final HostRecordInfo item = list.get(position);

		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.host_item, parent, false);
		}
		HostsAdapterHolder holder = (HostsAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new HostsAdapterHolder();
			holder.tvItemIP = (TextView) v.findViewById(R.id.tvItem_IP);
			holder.tvItemDomain = (TextView) v.findViewById(R.id.tvItem_Domain);
			holder.chkItemChecked = (CheckBox) v
					.findViewById(R.id.chkItem_Checked);
			holder.imgLocked = (ImageView) v.findViewById(R.id.imgLocked);
			v.setTag(holder);
		}

		if (item != null) {
			holder.tvItemIP.setText(item.ip);
			holder.tvItemDomain.setText(item.domain);
			holder.chkItemChecked.setVisibility(showCheckbox ? View.VISIBLE
					: View.GONE);

			if (locked0) {
				if (!showCheckbox) {
					holder.chkItemChecked.setVisibility(View.GONE);
				} else {
					boolean needLock = (position == 0
							&& item.ip.equals("127.0.0.1") && item.domain
							.equals("localhost"));
					holder.chkItemChecked.setVisibility(needLock ? View.GONE
							: View.VISIBLE);
					holder.imgLocked.setVisibility(needLock ? View.VISIBLE
							: View.GONE);
				}
			}

			holder.chkItemChecked.setChecked(item.checked);
			holder.chkItemChecked.setOnClickListener(new OnClickListener() {

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
					ArrayList<HostRecordInfo> l = new ArrayList<HostRecordInfo>(
							list);
					results.values = l;
					results.count = l.size();
				}
			} else {

				String prefixString = prefix.toString().toLowerCase();

				final ArrayList<HostRecordInfo> values = new ArrayList<HostRecordInfo>(
						list);

				final int count = values.size();

				final ArrayList<HostRecordInfo> newValues = new ArrayList<HostRecordInfo>(
						count);

				for (int i = 0; i < count; i++) {
					final HostRecordInfo value = values.get(i);
					String valueText = value.ip + value.domain;

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
			list = (List<HostRecordInfo>) results.values;
			if (results.count > 0) {
				notifyDataSetChanged();
			} else {
				notifyDataSetInvalidated();
			}
		}

	}
	// [/region]
}
