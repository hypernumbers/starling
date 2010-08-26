# Build file for Starling
# Hasan Veldstra <hasan@12monkeys.co.uk>

CC = (`icu-config --cc` + `icu-config --cflags` +
      "-std=c99 -pedantic -Wall").gsub(/\n/, " ")

BLD = "ebin"
SRC = "src"
C_SRC = "c_src"
ERL_FILES = Dir["#{SRC}/*.erl"].join(" ")
DRV_FILES = Dir["#{C_SRC}/*.c"].join(" ")

ICU_INCLUDE_FLAGS = `icu-config --cppflags-searchpath`.strip
ICU_LD_FLAGS = (`icu-config --ldflags` +
                `icu-config --ldflags-icuio`).gsub(/\n/, " ")

EI_INCLUDE_FLAGS = "-I#{`./findei.erl`.strip}/include"
EI_LD_FLAGS = "-L#{`./findei.erl`.strip}/lib -lei -lerl_interface"

task :default => [:app, :drv]

task(:app) do
  sh %{erlc -o #{BLD} #{ERL_FILES}} do |ok, res|
    if !ok
      puts "erlc error, status = #{res.exitstatus}"
    end
  end
end

task(:drv) do
  sh %{#{CC} #{DRV_FILES} -o #{File.join(BLD, "starling_drv")} #{ICU_INCLUDE_FLAGS} #{EI_INCLUDE_FLAGS} #{ICU_LD_FLAGS} #{EI_LD_FLAGS}} do |ok, res|
    if !ok
      puts "gcc error, status = #{res.exitstatus}"
    end
  end
end

task(:clean) do
  Dir["ebin/*.beam"].each { |x| rm x rescue nil }
  rm File.join("ebin", "starling_drv") rescue nil
end
