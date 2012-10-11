package com.rarnu.findaround.adapter;

import java.util.List;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.baidu.mapapi.GeoPoint;
import com.rarnu.findaround.GlobalInstance;
import com.rarnu.findaround.R;

public class PoiAdapter extends BaseAdapter {

	private static double DEF_PI = 3.14159265359; // PI
	private static double DEF_2PI = 6.28318530712; // 2*PI
	private static double DEF_PI180 = 0.01745329252; // PI/180.0
	private static double DEF_R = 6370693.5; // radius of earth

	private LayoutInflater inflater;
	private List<PoiInfoEx> list;
	private Context context;
	private Handler h;

	public PoiAdapter(Context context, LayoutInflater inflater,
			List<PoiInfoEx> list, Handler h) {
		this.context = context;
		this.inflater = inflater;
		this.list = list;
		this.h = h;
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
			v = inflater.inflate(R.layout.poi_item, parent, false);
		}
		PoiHolder holder = (PoiHolder) v.getTag();
		if (holder == null) {
			holder = new PoiHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			holder.tvAddress = (TextView) v.findViewById(R.id.tvAddress);
			holder.tvDistance = (TextView) v.findViewById(R.id.tvDistance);
			holder.btnDistance = (Button) v.findViewById(R.id.btnDistance);
			holder.imgSearch = (ImageView) v.findViewById(R.id.imgSearch);
			v.setTag(holder);
		}
		PoiInfoEx item = list.get(position);
		if (item != null) {
			holder.tvName.setText(item.info.name);
			holder.tvAddress.setText(item.info.address);
			holder.tvDistance.setText(getDistance(item.info.pt));
			holder.btnDistance.setVisibility(item.showButton ? View.VISIBLE
					: View.GONE);
			holder.imgSearch.setVisibility(item.showButton ? View.VISIBLE
					: View.GONE);
			holder.tvName.setTextColor(item.showButton ? 0xFFDA4E00
					: 0xFF000000);
			holder.btnDistance.setOnClickListener(new OnClickListener() {

				@Override
				public void onClick(View v) {
					Message msg = new Message();
					msg.what = 1;
					msg.arg1 = position;
					h.sendMessage(msg);

				}
			});
		}
		return v;
	}

	private String getDistance(GeoPoint geo) {
		double lat = (double) (((double) geo.getLatitudeE6()) / 1e6);
		double lng = (double) (((double) geo.getLongitudeE6()) / 1e6);
		double dist = getDistance(lng, lat, GlobalInstance.pointOri.longitude,
				GlobalInstance.pointOri.latitude);
		int iDist = (int) dist;
		return String.valueOf(iDist) + context.getString(R.string.meter);
	}

	private double getDistance(double lon1, double lat1, double lon2,
			double lat2) {
		double ew1, ns1, ew2, ns2;
		double dx, dy, dew;
		double distance;

		ew1 = lon1 * DEF_PI180;
		ns1 = lat1 * DEF_PI180;
		ew2 = lon2 * DEF_PI180;
		ns2 = lat2 * DEF_PI180;
		dew = ew1 - ew2;
		if (dew > DEF_PI)
			dew = DEF_2PI - dew;
		else if (dew < -DEF_PI)
			dew = DEF_2PI + dew;
		dx = DEF_R * Math.cos(ns1) * dew;
		dy = DEF_R * (ns1 - ns2);

		distance = Math.sqrt(dx * dx + dy * dy);
		return distance;
	}
}
