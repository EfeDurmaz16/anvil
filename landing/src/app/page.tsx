import Header from "@/components/Header";
import Hero from "@/components/Hero";
import StatsBanner from "@/components/StatsBanner";
import HowItWorks from "@/components/HowItWorks";
import InteractiveDemo from "@/components/InteractiveDemo";
import BenchmarkResults from "@/components/BenchmarkResults";
import Architecture from "@/components/Architecture";
import CodeComparison from "@/components/CodeComparison";
import FeatureGrid from "@/components/FeatureGrid";
import QuickStart from "@/components/QuickStart";
import OpenSourceCTA from "@/components/OpenSourceCTA";
import Footer from "@/components/Footer";

export default function Home() {
  return (
    <main className="flex flex-col">
      <Header />
      <Hero />
      <StatsBanner />
      <HowItWorks />
      <InteractiveDemo />
      <BenchmarkResults />
      <Architecture />
      <CodeComparison />
      <FeatureGrid />
      <QuickStart />
      <OpenSourceCTA />
      <Footer />
    </main>
  );
}
