package com.rarnu.zoe.loving.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.common.DataInfo;
import com.rarnu.zoe.loving.event.OnHistoryClick;

public class HistoryAdapter extends BaseAdapter {

	private Context context;
	private List<DataInfo> list;
	private LayoutInflater inflater;
	private OnHistoryClick click;
	private int itemWidth;
	private int itemHeight;

	public HistoryAdapter(Context context, List<DataInfo> list, int itemWidth, int itemHeight,
			OnHistoryClick click) {
		this.context = context;
		this.list = list;
		this.itemWidth = itemWidth;
		this.itemHeight = itemHeight;
		this.click = click;
		this.inflater = LayoutInflater.from(context);
	}

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
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_history, parent, false);
		}
		HistoryHolder holder = (HistoryHolder) v.getTag();
		if (holder == null) {
			holder = new HistoryHolder();
			holder.tvItem1 = (TextView) v.findViewById(R.id.tvItem1);
			holder.tvItem2 = (TextView) v.findViewById(R.id.tvItem2);
			holder.tvItem3 = (TextView) v.findViewById(R.id.tvItem3);
			holder.imgItem1 = (ImageView) v.findViewById(R.id.imgItem1);
			holder.imgItem2 = (ImageView) v.findViewById(R.id.imgItem2);
			holder.imgItem3 = (ImageView) v.findViewById(R.id.imgItem3);

			holder.tvItem1.setTag(1);
			holder.tvItem2.setTag(2);
			holder.tvItem3.setTag(3);

			holder.setComponentSize(itemWidth);
			v.setTag(holder);
		}
		DataInfo item = list.get(position);
		if (item != null) {

			holder.imgItem1.setVisibility(item.data1 <=7 ? View.VISIBLE
					: View.GONE);
			holder.imgItem2.setVisibility(item.data2 <=7 ? View.VISIBLE
					: View.GONE);
			holder.imgItem3.setVisibility(item.data3 <=7 ? View.VISIBLE
					: View.GONE);

			holder.tvItem1.setText(item.data1 == 99 ? "" : context
					.getString(R.string.emotion2));
			holder.tvItem2.setText(item.data2 == 99 ? "" : context
					.getString(R.string.emotion2));
			holder.tvItem3.setText(item.data3 == 99 ? "" : context
					.getString(R.string.emotion2));

			OnClickListener listener = new OnClickListener() {

				@Override
				public void onClick(View v) {
					if (click != null) {
						click.onClick(v,
								((position * 3) + (Integer) v.getTag()));
					}

				}
			};
			holder.tvItem1.setOnClickListener(listener);
			holder.tvItem2.setOnClickListener(listener);
			holder.tvItem3.setOnClickListener(listener);

		}
		ViewGroup.LayoutParams vlp = v.getLayoutParams();
		vlp.height = itemHeight;
		v.setLayoutParams(vlp);
		return v;
	}

}
