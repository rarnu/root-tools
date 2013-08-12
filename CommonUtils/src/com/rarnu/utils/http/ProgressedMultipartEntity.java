package com.rarnu.utils.http;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.http.params.HttpParams;

public class ProgressedMultipartEntity extends MultipartEntity {

	public interface ProgressListener {
		void onProgressChanged(long bytes);
	}

	private ProgressListener listener;

	public ProgressedMultipartEntity(Part[] parts, ProgressListener listener) {
		super(parts);
		this.listener = listener;
	}

	public ProgressedMultipartEntity(Part[] parts, HttpParams params,
			ProgressListener listener) {
		super(parts, params);
		this.listener = listener;
	}

	@Override
	public void writeTo(final OutputStream outstream) throws IOException {
		super.writeTo(new CountingOutputStream(outstream, this.listener));
	}

	public static class CountingOutputStream extends FilterOutputStream {

		private final ProgressListener callback;
		private long transferred;

		public CountingOutputStream(final OutputStream out,
				final ProgressListener callback) {
			super(out);
			this.callback = callback;
			this.transferred = 0;
		}

		public void write(byte[] b, int off, int len) throws IOException {
			out.write(b, off, len);
			transferred += len;
			if (callback != null) {
				callback.onProgressChanged(transferred);
			}
		}

		public void write(int b) throws IOException {
			out.write(b);
			transferred++;
			if (callback != null) {
				callback.onProgressChanged(transferred);
			}
		}
	}

}
