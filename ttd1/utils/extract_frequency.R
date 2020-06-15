extract_freqs <- function(data, dataAcquisitionFreq=1) {
  # Extract dominant freq from a signal
  # @param data: vector of data signal
  # @param dataAcquisitionFreq: inverse of sample rate. For example,
  #   for PP collector device, the sample rate is about 8 samples/s
  #   and the dataAcquisitionFreq is 8Hz accordingly.
  # @returns a vector of isolated frequencies
  
  # Compute length and time of the original signal
  data_len <- length(data)
  time <- data_len / dataAcquisitionFreq
  
  # Transform signal into freq space
  data_fft <- fft(data, inverse = F)
  freq_magn <- Mod(data_fft)
  
  # Select only first half of vectors
  freq_magn <- freq_magn[1:(length(freq_magn)/2)]
  
  # Select freq having magnitude higher than threshold
  # TODO: Improve the way of calculating this threshold
  thres_hold <- 0.001
  return(sort(which(freq_magn > thres_hold) / time))
}

similariry <- function(v1, v2) {
  # Compute similariry of 2 vectors
  return(sum(v1*v2)/sqrt(sum(v1^2)*sum(v2^2)))
}

test_extract_freqs <- function(da.freq=2000, signal.freqs=c(50, 130), time=5) {
  # Test the function extract_freq
  # - Create and simulate a combining signal of n isolated signals
  # - Apply the function extract_freqs
  
  # @param da.freq: data acquisition frequency in Hz
  # @param signal.freqs: vector of frequencies of nth signal in Hz
  # @param time: in seconds
  
  if (length(which(signal.freqs > da.freq)) > 0) {
    stop("Signal freqs must be less than data acquisition freq.")
  }
  
  # Order
  signal.freqs <- sort(signal.freqs)
  
  # Vector of sampling in time space
  smpl.int <- (1:(time*da.freq))/da.freq  
  
  # Combine 2 signals
  data <- NULL
  for (freq in signal.freqs) {
    radian_diff = pi / round(runif(1) * 9 + 1)
    if (is.null(data)) {
      data <- sin(freq*smpl.int*2*pi + radian_diff)
    } else {
      data <- data + sin(freq*smpl.int*2*pi+ radian_diff)
    }
  }
  
  # Test the function
  freqs <- extract_freqs(data, dataAcquisitionFreq = da.freq)
  
  print(paste0("Original Freqs: ", paste0(signal.freqs, collapse = ', ')))
  print(paste0("Extracted Freqs: ", paste0(freqs, collapse = ', ')))
  
  # Tol threshold
  tol <- 0.01
  
  if (length(freqs) == length(signal.freqs)) {
    sim <- similariry(signal.freqs, freqs)
    if (1 - sim < tol) {
      print(paste0("Well done! Approximate: ",  sim * 100, "% to original freqs")) 
    } else {
      stop(paste0('Inaccurate extraction.'))
    }
  } else {
    stop('Inaccurate extraction. Lost Freqs.')
  }
}

# Test with high freq signals
test_extract_freqs(da.freq=2000, signal.freqs = c(50, 100), time=5)
test_extract_freqs(da.freq=4000, signal.freqs = c(25, 120, 155), time=50)

# Test with low freq signals
# Notes: Please remember for simulated signals, signal frequencies must be less than 
# data acquisition frequencies.
test_extract_freqs(da.freq=10, signal.freqs = c(0.5, 1, 2, 4), time=900)
test_extract_freqs(da.freq=10, signal.freqs = c(0.5, 1, 2, 3, 4), time=900)



